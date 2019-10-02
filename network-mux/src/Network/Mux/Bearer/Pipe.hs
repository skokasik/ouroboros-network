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
  , pipeChannelFromHANDLES
#endif
  , pipeAsMuxBearer
  , runMuxWithPipes
  ) where

import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Tracer
import           Data.Word
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (traverse_)
import           GHC.Stack
import           System.IO (Handle, hFlush)

#if defined(mingw32_HOST_OS)
import qualified System.Win32.Types      as Win32 (HANDLE)
import qualified System.Win32.NamedPipes as Win32
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
pipeChannelFromHANDLES :: Win32.HANDLE
                       -- ^ read handle
                       -> Win32.HANDLE
                       -- ^ write handle
                       -> PipeChannel
pipeChannelFromHANDLES r w = PipeChannel {
      readHandle  = fmap BL.fromStrict . Win32.readPipe r,
      writeHandle = traverse_ (Win32.writePipe w) . BL.toChunks,
      flushHandle = pure () -- TODO
    }
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
    let muxTracer = Mx.WithMuxBearer "Pipe" `contramap` tracer
    bearer <- pipeAsMuxBearer muxTracer channel
    Mx.muxStart muxTracer peerid app bearer

