{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Async.Handle (tests) where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Concurrent
import           Control.Exception
import           Control.Monad (when)
import           Data.Functor (void)
import           Data.Bits
import           Data.Binary (Binary (..), encode, decode)
import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable (foldl', traverse_)

import           System.Win32

import           System.Win32.NamedPipes
import           System.Win32.Async
import           Test.Generators hiding (tests)
import           Test.Async.PingPong

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck (testProperty)
import           Test.QuickCheck
import           Test.QuickCheck.Instances.ByteString ()

pipeName :: String
pipeName = "\\\\.\\pipe\\test-Win32-network-async"

tests :: TestTree
tests =
  testGroup "Async.Handle"
  [ testCase "interruptible connectNamedPipe"
      test_interruptible_connectNamedPipe
  , testCase "interruptible readHandle"
      test_interruptible_readHandle
  , testCase "interruptible readHandle_2"
      test_interruptible_readHandle_2
  , testCase "interruptible writeHandle"
      test_interruptible_writeHandle
  , testProperty "async reads and writes"
      prop_async_read_and_writes
  , testProperty "PingPong test"
      prop_PingPong
  , testProperty "PingPongPipelined test"
      $ withMaxSuccess 50 prop_PingPongPipelined
  ]


--
-- Unit tests
--

-- | This test would fail (deadlock) if the blocking call `connectNamedPipe`,
-- would not be interruptible.
--
test_interruptible_connectNamedPipe :: IO ()
test_interruptible_connectNamedPipe = withIOManager $ \iocp ->
    bracket (createNamedPipe pipeName
                             (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             512
                             512
                             0
                             Nothing)
            closeHandle
            $ \hpipe -> do
                _ <- associateWithIOCompletionPort (Left hpipe) iocp
                tid <- forkIO (connectNamedPipe hpipe)
                threadDelay 100
                killThread tid

-- | Check if 'readHandle'`is interruptible
--
test_interruptible_readHandle :: IO ()
test_interruptible_readHandle = withIOManager $ \iocp ->
    bracket ((,) <$> createNamedPipe pipeName
                                     (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                                     (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                                     pIPE_UNLIMITED_INSTANCES
                                     512
                                     512
                                     0
                                     Nothing
                 <*> createFile pipeName
                                gENERIC_READ
                                fILE_SHARE_NONE
                                Nothing
                                oPEN_EXISTING
                                fILE_FLAG_OVERLAPPED
                                Nothing)
            (\(x, y) -> closeHandle x >> closeHandle y)
            $ \(hpipe, hpipe') -> do
                _ <- associateWithIOCompletionPort (Left hpipe)  iocp
                _ <- associateWithIOCompletionPort (Left hpipe') iocp
                tid <- forkIO (void $ readHandle hpipe' 1)
                threadDelay 100
                killThread tid

-- | Interrupt two simultanous reads.
--
test_interruptible_readHandle_2 :: IO ()
test_interruptible_readHandle_2 = withIOManager $ \iocp -> do
    bracket ((,) <$> createNamedPipe pipeName
                                     (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                                     (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                                     pIPE_UNLIMITED_INSTANCES
                                     512
                                     512
                                     0
                                     Nothing
                 <*> createFile pipeName
                                (gENERIC_READ .|. gENERIC_WRITE)
                                fILE_SHARE_NONE
                                Nothing
                                oPEN_EXISTING
                                fILE_FLAG_OVERLAPPED
                                Nothing)
            (\(x, y) -> closeHandle x >> closeHandle y)
            $ \(hpipe, hpipe') -> do
              _ <- associateWithIOCompletionPort (Left hpipe)  iocp
              _ <- associateWithIOCompletionPort (Left hpipe') iocp
              tid  <- forkIO (void $ readHandle hpipe' 1)
              tid' <- forkIO (void $ readHandle hpipe' 1)
              threadDelay 100
              killThread tid
              killThread tid'


test_interruptible_writeHandle :: IO ()
test_interruptible_writeHandle = withIOManager $ \iocp -> do
    let bs = BSC.pack $ replicate 100 'a'
    v <- newEmptyMVar
    syncVar <- newEmptyMVar

    bracket
      ((,) <$> createNamedPipe pipeName
                               (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                               (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                               pIPE_UNLIMITED_INSTANCES
                               1
                               1
                               0
                               Nothing
           <*> createFile pipeName
                          gENERIC_WRITE
                          fILE_SHARE_NONE
                          Nothing
                          oPEN_EXISTING
                          fILE_FLAG_OVERLAPPED
                          Nothing)
      (\(x, y) -> closeHandle x >> closeHandle y)
      $ \(r, w) -> do
        _ <- associateWithIOCompletionPort (Left r) iocp
        _ <- associateWithIOCompletionPort (Left w) iocp

        tid <- forkIOWithUnmask $ \unmask ->
          void $ do
            putMVar syncVar ()
            unmask (writeHandle w bs)
          `catch` \(e :: AsyncException) -> putMVar v e >> throwIO e

        -- wait for 'writeHandle'
        takeMVar syncVar
        threadDelay 100
        killThread tid

        e <- takeMVar v
        assertBool "test_interruptible_writeHandle" (ThreadKilled == e)


--
-- QuickCheck tests
--

-- | Run a server and client which both simultanously read and write from a
-- handle.
--
prop_async_read_and_writes :: LargeNonEmptyBS
                           -> LargeNonEmptyBS
                           -> Property
prop_async_read_and_writes (LargeNonEmptyBS bsIn bufSizeIn) (LargeNonEmptyBS bsOut bufSizeOut) =
    ioProperty $ withIOManager $ \iocp -> do
      threadDelay 100

      syncVarStart <- newEmptyMVar
      syncVarEnd   <- newEmptyMVar
      clientVar <- newEmptyMVar
      serverVar <- newEmptyMVar

      -- fork a server
      _ <- forkIO $
        bracket
            (createNamedPipe pname
                             (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                             (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                             pIPE_UNLIMITED_INSTANCES
                             (fromIntegral bufSizeIn)
                             (fromIntegral bufSizeOut)
                             0
                             Nothing)
            closeHandle
            $ \h -> do
              -- associate 'h' with  I/O completion 'port'
              _ <- associateWithIOCompletionPort (Left h) iocp
              putMVar syncVarStart ()
              connectNamedPipe h
              void $ forkIO $
                readHandle h (BS.length bsIn)
                  >>= putMVar serverVar
              void $ forkIO $ writeHandle h bsOut
              takeMVar syncVarEnd


      -- fork a client
      _ <- forkIO $ do
        takeMVar syncVarStart
        bracket
          (createFile pname
                      (gENERIC_READ .|. gENERIC_WRITE)
                      fILE_SHARE_NONE
                      Nothing
                      oPEN_EXISTING
                      fILE_FLAG_OVERLAPPED
                      Nothing)
          closeHandle
          $ \h -> do
            -- associate 'h' with  I/O completion 'port'
            _ <- associateWithIOCompletionPort (Left h) iocp
            readerAsync <- async $
              readHandle h (BS.length bsOut)
                >>= putMVar clientVar
            writerAsync <- async $ writeHandle h bsIn
            _ <- waitBoth readerAsync writerAsync
            putMVar syncVarEnd ()

      bsOut' <- takeMVar clientVar
      bsIn'  <- takeMVar serverVar

      pure $ bsIn == bsIn' && bsOut == bsOut'

  where
    pname = pipeName ++ "-reads-and-writes"

--
-- PingPong tests
--

-- | Send & receive from a pipe using a simple framing (header is the size of
-- the payload).  Note that the pipe does buffering of ingress and egress
-- bytes, and thus both operation can block.
--
handleToBinaryChannel :: Binary a
                      => HANDLE
                      -> BinaryChannel a
handleToBinaryChannel h = BinaryChannel { readChannel, writeChannel, closeChannel }
    where
      -- send all chunks through the pipe
      writeChannel b a = do
        let chunks :: [ByteString]
            chunks = BSL.toChunks (encode a)
            size   :: Int
            size   = bool (+1) id b $ foldl' (\x y -> x + BS.length y) 0 chunks
        -- send header: just a single chunk send payload
        _ <- writeHandle h (BSL.toStrict $ encode size)
        traverse_ (\chunk -> writeHandle h chunk) chunks

      readChannel b = do
        bs <- readLen [] 8
        if b
          then do
            let s = decode (BSL.fromStrict bs)
            bs' <- readLen [] s
            pure $ Just $ decode $ BSL.fromStrict $ bs'
          else pure Nothing

      readLen !bufs 0 = pure $ BS.concat (reverse bufs)
      readLen !bufs s = do
        bs <- readHandle h s
        when (BS.null bs)
          $ throwIO ReceivedNullBytes
        readLen (bs : bufs) (s - fromIntegral (BS.length bs))
        
      -- we close the handle explicitely
      closeChannel = closeHandle h

      {-
      acceptChannel = connectNamedPipe h $> BinaryChannel { readChannel
                                                          , writeChannel
                                                          , closeChannel
                                                          , acceptChannel = error "cannot accept this channel"
                                                          }
      -}


-- | Stress test for named pipe ffi calls.
--
-- For each entry in @NonEmptyList (NonNegative Int)@ we run a single instance
-- of a ping pong protocol which will exchange that many messages.  Each
-- instance runs on it's own named pipe.  When the client sends its last message, it
-- misinforms the server about the size of the message, and kills the server
-- thread.  This ensure that when we kill the server it is blocked on reading.
--
prop_PingPong :: Int
              -- ^ the number of messages exchanged in the ping pong protocol
              -> Blocking
              -> LargeNonEmptyBS
              -> Property
prop_PingPong n blocking (LargeNonEmptyBS bs bufSize) =
    ioProperty $ withIOManager $ \iocp -> do
      let pname = pipeName ++ "-ping-pong"

      -- fork the PingPong server
      h <- createNamedPipe pname
                           (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                           (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                           pIPE_UNLIMITED_INSTANCES
                           (fromIntegral bufSize)
                           (fromIntegral bufSize)
                           0
                           Nothing
      associateWithIOCompletionPort (Left h) iocp
      let channel = handleToBinaryChannel h
      lock <- newEmptyMVar
      tid <- mask_ $ forkIOWithUnmask $ \unmask ->
        do
          connectNamedPipe h
          unmask (runPingPongServer channel (constPingPongServer @ByteString))
        -- TODO: this finally is really needed against the whole block, sometimes the async exception must hit `connectNamedPipe`.
        `finally` putMVar lock ()

      -- run the PingPong client
      h' <- createFile pname
                       (gENERIC_READ .|. gENERIC_WRITE)
                       fILE_SHARE_NONE
                       Nothing
                       oPEN_EXISTING
                       fILE_FLAG_OVERLAPPED
                       Nothing
      associateWithIOCompletionPort (Left h') iocp
      let channel' = handleToBinaryChannel h'
      res <- runPingPongClient channel' blocking tid (constPingPongClient n bs)

      -- await until server is killed
      takeMVar lock
      closeChannel channel
      closeChannel channel'

      pure $ case blocking of
        NonBlocking -> res == replicate n bs
        _           -> res == replicate (pred n) bs


--
-- Pipelined PingPong test
--


prop_PingPongPipelined :: Blocking
                       -> Positive Int
                       -> NonEmptyList LargeNonEmptyBS
                       -- non empty list of requests
                       -> Property
prop_PingPongPipelined blocking (Positive bufSize) (NonEmpty bss0) =
    ioProperty $ withIOManager $ \iocp -> do

      let bss = map getLargeNonEmptyBS bss0
          pname = pipeName ++ "-ping-pong-pipelined"

      -- fork the PingPong server
      h <- createNamedPipe pname
                           (pIPE_ACCESS_DUPLEX .|. fILE_FLAG_OVERLAPPED)
                           (pIPE_TYPE_BYTE .|. pIPE_READMODE_BYTE)
                           pIPE_UNLIMITED_INSTANCES
                           (fromIntegral bufSize)
                           maxBound
                           0
                           Nothing
      associateWithIOCompletionPort (Left h) iocp
      let channel = handleToBinaryChannel h
      lock <- newEmptyMVar
      tid <- mask_ $ forkIOWithUnmask $ \unmask ->
        do
          connectNamedPipe h
          unmask (runPingPongServer channel (constPingPongServer @ByteString))
        `finally` putMVar lock ()

      -- run the PingPong client
      h' <- createFile pname
                       (gENERIC_READ .|. gENERIC_WRITE)
                       fILE_SHARE_NONE
                       Nothing
                       oPEN_EXISTING
                       fILE_FLAG_OVERLAPPED
                       Nothing
      associateWithIOCompletionPort (Left h') iocp
      let channel' = handleToBinaryChannel h'
      res <- runPingPongClientPipelined channel' blocking tid bss

      takeMVar lock
      closeChannel channel
      closeChannel channel'
      case blocking of
        NonBlocking -> case res of
          Just bss' -> pure $ bss == bss'
          Nothing   -> pure False
        _           -> pure True -- if we evalute this case branch, it means that
                                 -- killing blocked thread did not deadlock.
