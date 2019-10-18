{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mux.Bearer.Pipe (
    PipeChannel (..)
  , pipeChannelFromHandles
#if defined(mingw32_HOST_OS)
  -- , winio_pipeChannelFromHANDLE
  , pipeChannelFromHANDLE
  , pipeChannelFromHANDLE_Ev
#endif
  , pipeAsMuxBearer
  , runMuxWithPipes
  ) where

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer
import           Data.Word
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS (unsafeUseAsCStringLen)
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (traverse_)
import           GHC.Stack
import           System.IO (Handle, hFlush)

#if defined(mingw32_HOST_OS)
import           Foreign (castPtr)
import           Foreign.Marshal.Utils (with)
import           System.Win32.Types                   (HANDLE)
import qualified System.Win32.Types          as Win32 (failIfFalse_)
import           System.Win32.Types.Overlapped        (OVERLAPPED (..))
import qualified System.Win32.Event          as Win32
import qualified System.Win32.CompletionPort as Win32
import qualified System.Win32.Async          as Win32.Async
import qualified System.Win32.NamedPipes     as Win32.NamedPipes
#endif

import qualified Network.Mux as Mx
import           Network.Mux.Types (MuxBearer)
import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Codec as Mx
import qualified Network.Mux.Interface as Mx
import qualified Network.Mux.Time as Mx

-- | Abstraction over various types of handles.  We provide two instances:
--
--  * based on 'Handle': os independepnt, but will not work well on Windows,
--  * based on 'Win32.HANDLE': Windows specific.
--
data PipeChannel = PipeChannel {
    readHandle  :: Int -> IO BL.ByteString,
    writeHandle :: BL.ByteString -> IO (),
    flushHandle :: IO ()
    -- ^ flush the write handle
  }

pipeChannelFromHandles :: Handle
                       -- ^ read handle
                       -> Handle
                       -- ^ write handle
                       -> PipeChannel
pipeChannelFromHandles r w = PipeChannel {
    readHandle  = BL.hGet r,
    writeHandle = BL.hPut w,
    flushHandle = hFlush w
  }


#if defined(mingw32_HOST_OS)
pipeChannelFromHANDLE :: HANDLE
                      -- ^ file handle
                      -> HANDLE
                      -- ^ I/O completion port
                      -> IO PipeChannel
pipeChannelFromHANDLE h port = do
    -- todo: close the thread
    _ <- asyncBound $ do
      Win32.Async.handleCompletions port
        `catch` (\(e :: SomeException) -> print e >> throwM e)
    Win32.failIfFalse_ "pipeChannelFromHANDLE.createCompletionPort"
      $ (== port) <$> Win32.createIoCompletionPort h port 0 0
    return PipeChannel {
        readHandle  = fmap BL.fromStrict . readPipe,
        writeHandle = traverse_ writePipe . BL.toChunks,
        flushHandle = pure ()
      }
  where
    readPipe :: Int -> IO ByteString
    readPipe = Win32.Async.read h

    writePipe :: ByteString -> IO ()
    writePipe = Win32.Async.write h
#endif

#if defined(mingw32_HOST_OS)
pipeChannelFromHANDLE_Ev :: HANDLE
                         -> IO PipeChannel
pipeChannelFromHANDLE_Ev h = do
    evRead  <- Win32.createEvent Nothing False False "evRead"
    evWrite <- Win32.createEvent Nothing False False "evWrite"
    pure PipeChannel
         { readHandle  = readHandle_ evRead
         , writeHandle = traverse_ (writeHandle_ evWrite) . BL.toChunks
         , flushHandle = pure ()
         }
  where
    readHandle_ :: HANDLE
                -> Int
                -> IO BL.ByteString
    readHandle_ ev size = BL.fromStrict <$> BS.createAndTrim size
      (\ptr -> do
        let ovl = OVERLAPPED
              { ovl_internal     = 0
              , ovl_internalHigh = 0
              , ovl_offset       = 0
              , ovl_offsetHigh   = 0
              , ovl_hEvent       = ev
              }
        _ <- Win32.NamedPipes.win32_ReadFile h ptr (fromIntegral size) (Just ovl)
        _ <- Win32.waitForSingleObject ev maxBound
        -- TODO: how to get the correct size?
        pure size)

    writeHandle_ :: HANDLE
                 -> ByteString
                 -> IO ()
    writeHandle_ ev bs = BS.unsafeUseAsCStringLen bs $
      \(str, len) ->
        let ovl = OVERLAPPED
              { ovl_internal     = 0
              , ovl_internalHigh = 0
              , ovl_offset       = 0
              , ovl_offsetHigh   = 0
              , ovl_hEvent       = ev
              }
        in with ovl $ \ovl_ptr -> do
          _ <- Win32.NamedPipes.win32_WriteFile h (castPtr str) (fromIntegral len) ovl_ptr
          _ <- Win32.waitForSingleObject ev maxBound
          pure ()
#endif

pipeAsMuxBearer
  :: forall ptcl.
     ( Mx.ProtocolEnum ptcl
     , Ord ptcl
     , Enum ptcl
     , Bounded ptcl
     )
  => Tracer IO (Mx.MuxTrace ptcl)
  -> PipeChannel
  -> IO (MuxBearer ptcl IO)
pipeAsMuxBearer tracer channel = do
      mxState <- atomically $ newTVar Mx.Larval
      return $ Mx.MuxBearer {
          Mx.read = readPipe,
          Mx.write = writePipe,
          Mx.sduSize = sduSize,
          Mx.state = mxState
        }
    where
      readPipe :: (HasCallStack)
               => IO (Mx.MuxSDU ptcl, Time)
      readPipe = do
          traceWith tracer $ Mx.MuxTraceRecvHeaderStart
          hbuf <- recvLen' 8 []
          case Mx.decodeMuxSDU hbuf of
              Left e     -> throwM e
              Right header -> do
                  --say $ printf "decoded mux header, goint to read %d bytes" (Mx.msLength header)
                  traceWith tracer $ Mx.MuxTraceRecvHeaderEnd header
                  traceWith tracer $ Mx.MuxTraceRecvPayloadStart (fromIntegral $ Mx.msLength header)
                  blob <- recvLen' (fromIntegral $ Mx.msLength header) []
                  ts <- getMonotonicTime
                  traceWith tracer $ Mx.MuxTraceRecvPayloadEnd blob
                  --hexDump blob ""
                  return (header {Mx.msBlob = blob}, ts)

      recvLen' :: Int -> [BL.ByteString] -> IO BL.ByteString
      recvLen' 0 bufs = return $ BL.concat $ reverse bufs
      recvLen' l bufs = do
          traceWith tracer $ Mx.MuxTraceRecvStart l
          buf <- readHandle channel l
          if BL.null buf
              then throwM $ Mx.MuxError Mx.MuxBearerClosed "Pipe closed when reading data" callStack
              else do
                  traceWith tracer $ Mx.MuxTraceRecvEnd buf
                  recvLen' (l - fromIntegral (BL.length buf)) (buf : bufs)

      writePipe :: Mx.MuxSDU ptcl
                -> IO Time
      writePipe sdu = do
          ts <- getMonotonicTime
          let ts32 = Mx.timestampMicrosecondsLow32Bits ts
              sdu' = sdu { Mx.msTimestamp = Mx.RemoteClockModel ts32 }
              buf  = Mx.encodeMuxSDU sdu'
          traceWith tracer $ Mx.MuxTraceSendStart sdu'
          writeHandle channel buf
          flushHandle channel
          traceWith tracer $ Mx.MuxTraceSendEnd
          return ts

      sduSize :: IO Word16
      sduSize = return 32768

runMuxWithPipes
    :: ( Mx.ProtocolEnum ptcl, Ord ptcl, Enum ptcl, Bounded ptcl, Show ptcl
       , Mx.MiniProtocolLimits ptcl)
    => Tracer IO (Mx.WithMuxBearer String (Mx.MuxTrace ptcl))
    -> peerid
    -> Mx.MuxApplication appType peerid ptcl IO a b
    -> PipeChannel
    -> IO ()
runMuxWithPipes tracer peerid app channel = do
    bearer <- pipeAsMuxBearer tracer channel
    Mx.muxStart tracer peerid app bearer

