{-# LANGUAGE CPP                        #-} 
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

{-# OPTIONS_GHC -Wno-orphans            #-}

module Test.Mux
    ( tests
    ) where

import           Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding as CBOR
import           Codec.Serialise (Serialise (..))
import           Control.Monad
import qualified Data.Binary.Put as Bin
import           Data.Bits
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8 (pack)
import           Data.List (dropWhileEnd)
import           Data.Int
import           Data.Tuple (swap)
import           Data.Word
import           Data.Void (Void) 
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.Tasty
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Printf
import qualified System.Random.SplitMix as SM

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadST
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Monad.IOSim (runSimStrictShutdown)
import           Control.Tracer (Tracer (..), contramap, nullTracer, showTracing)

#if defined(mingw32_HOST_OS)
import qualified System.Win32.NamedPipes as Win32
import qualified System.Win32.File       as Win32.File
#endif

import           Test.Mux.ReqResp

import qualified Network.Mux as Mx
import qualified Network.Mux.Types as Mx
import qualified Network.Mux.Codec as Mx
import qualified Network.Mux.Channel as Mx
import qualified Network.Mux.Interface as Mx
import qualified Network.Mux.Bearer.Queues as Mx
import qualified Network.Mux.Bearer.Pipe as Mx

stdoutTracer :: Tracer IO String
stdoutTracer = Tracer $ BS8.putStrLn . BS8.pack

tests :: TestTree
tests =
  testGroup "Mux"
  [ testProperty "mux send receive"      prop_mux_snd_recv
  , testProperty "1 miniprotocol Queue"  prop_mux_1_mini_Queue
  , testProperty "1 miniprotocol Pipe"   prop_mux_1_mini_Pipe
  , testProperty "2 miniprotocols Queue" prop_mux_2_minis_Queue
  , testProperty "2 miniprotocols Pipe"  prop_mux_2_minis_Pipe
  , testProperty "starvation"            prop_mux_starvation
  , testProperty "demuxing (Sim)"        prop_demux_sdu_sim
  , testProperty "demuxing (IO)"         prop_demux_sdu_io
  , testGroup "Generators"
    [ testProperty "genByteString"       prop_arbitrary_genByteString
    , testProperty "genLargeByteString"  prop_arbitrary_genLargeByteString
    ]
  ]

defaultMiniProtocolLimit :: Int64
defaultMiniProtocolLimit = 3000000

smallMiniProtocolLimit :: Int64
smallMiniProtocolLimit = 16*1024

activeTracer :: forall m a. (MonadSay m, Show a) => Tracer m a
activeTracer = nullTracer
--activeTracer = showTracing sayTracer

_sayTracer :: MonadSay m => Tracer m String
_sayTracer = Tracer say

--
-- Various ProtocolEnum instances used in tests
--

data TestProtocols1 = ReqResp1
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols1 where
  fromProtocolEnum ReqResp1 = 2

  toProtocolEnum 2 = Just ReqResp1
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits TestProtocols1 where
  maximumMessageSize ReqResp1  = defaultMiniProtocolLimit
  maximumIngressQueue ReqResp1 = defaultMiniProtocolLimit

-- |
-- Allows to run two copies of ReqResp protocol.t
--
data TestProtocols2 = ReqResp2 | ReqResp3
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocols2 where
  fromProtocolEnum ReqResp2 = 2
  fromProtocolEnum ReqResp3 = 3

  toProtocolEnum 2 = Just ReqResp2
  toProtocolEnum 3 = Just ReqResp3
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits TestProtocols2 where
  maximumMessageSize _  = defaultMiniProtocolLimit
  maximumIngressQueue _ = defaultMiniProtocolLimit

data TestProtocolsSmall = ReqRespSmall
  deriving (Eq, Ord, Enum, Bounded, Show)

instance Mx.ProtocolEnum TestProtocolsSmall where
  fromProtocolEnum ReqRespSmall = 2

  toProtocolEnum 2 = Just ReqRespSmall
  toProtocolEnum _ = Nothing

instance Mx.MiniProtocolLimits TestProtocolsSmall where
  maximumMessageSize ReqRespSmall  = smallMiniProtocolLimit
  maximumIngressQueue ReqRespSmall = smallMiniProtocolLimit

--
-- Generators
--

newtype DummyPayload = DummyPayload {
      unDummyPayload :: BL.ByteString
    } deriving Eq

instance Show DummyPayload where
    show d = printf "DummyPayload %d\n" (BL.length $ unDummyPayload d)

-- |
-- Generate a byte string of a given size.
--
genByteString :: Int64 -> Gen BL.ByteString
genByteString size = do
    g0 <- return . SM.mkSMGen =<< chooseAny
    return $ BL.unfoldr gen (size, g0)
  where
    gen :: (Int64, SM.SMGen) -> Maybe (Word8, (Int64, SM.SMGen))
    gen (!i, !g)
        | i <= 0    = Nothing
        | otherwise = Just (fromIntegral w64, (i - 1, g'))
      where
        !(w64, g') = SM.nextWord64 g

prop_arbitrary_genByteString :: (NonNegative (Small Int64)) -> Property
prop_arbitrary_genByteString (NonNegative (Small size)) = ioProperty $ do
  bs <- generate $ genByteString size
  return $ size == BL.length bs

genLargeByteString :: Int64 -> Int64 -> Gen BL.ByteString
genLargeByteString chunkSize  size | chunkSize < size = do
  chunk <- genByteString chunkSize
  return $ BL.concat $
        replicate (fromIntegral $ size `div` chunkSize) chunk
      ++
        [BL.take (size `mod` chunkSize) chunk]
genLargeByteString _chunkSize size = genByteString size

-- |
-- Large Int64 values, but not too large, up to @1024*1024@.
--
newtype LargeInt64 = LargeInt64 Int64
  deriving (Eq, Ord, Num, Show)

instance Arbitrary LargeInt64 where
    arbitrary = LargeInt64 <$> choose (1, 1024*1024)
    shrink (LargeInt64 n) = map LargeInt64 $ shrink n

prop_arbitrary_genLargeByteString :: NonNegative LargeInt64 -> Property
prop_arbitrary_genLargeByteString (NonNegative (LargeInt64 size)) = ioProperty $ do
  bs <- generate $ genLargeByteString 1024 size
  return $ size == BL.length bs

instance Arbitrary DummyPayload where
    arbitrary = do
        n <- choose (1, 128)
        len <- oneof [ return n
                     , return $ defaultMiniProtocolLimit - n - cborOverhead
                     , choose (1, defaultMiniProtocolLimit - cborOverhead)
                     ]
        -- Generating a completly arbitrary bytestring is too costly so it is only
        -- done for short bytestrings.
        DummyPayload <$> genLargeByteString 1024 len
      where
        cborOverhead = 7 -- XXX Bytes needed by CBOR to encode the dummy payload

instance Serialise DummyPayload where
    encode a = CBOR.encodeBytes (BL.toStrict $ unDummyPayload a)
    decode = DummyPayload . BL.fromStrict <$> CBOR.decodeBytes

-- | A sequences of dummy requests and responses for test with the ReqResp protocol.
newtype DummyTrace = DummyTrace {unDummyTrace :: [(DummyPayload, DummyPayload)]}
    deriving (Show)

instance Arbitrary DummyTrace where
    arbitrary = do
        len <- choose (1, 20)
        DummyTrace <$> vector len

data InvalidSDU = InvalidSDU {
      isTimestamp  :: !Mx.RemoteClockModel
    , isIdAndMode  :: !Word16
    , isLength     :: !Word16
    , isRealLength :: !Int64
    , isPattern    :: !Word8
    }

instance Show InvalidSDU where
    show a = printf "InvalidSDU 0x%08x 0x%04x 0x%04x 0x%04x 0x%02x\n"
                    (Mx.unRemoteClockModel $ isTimestamp a)
                    (isIdAndMode a)
                    (isLength a)
                    (isRealLength a)
                    (isPattern a)

data ArbitrarySDU = ArbitraryInvalidSDU InvalidSDU Mx.MuxBearerState Mx.MuxErrorType
                  | ArbitraryValidSDU DummyPayload Mx.MuxBearerState (Maybe Mx.MuxErrorType)
                  deriving Show

instance Arbitrary ArbitrarySDU where
    arbitrary = oneof [ unknownMiniProtocol
                      , invalidLenght
                      , validSdu
                      , tooLargeSdu
                      ]
      where
        validSdu = do
            b <- arbitrary

            -- Valid SDUs before version negotiation does only make sense for single SDUs.
            state <- if BL.length (unDummyPayload b) < 0xffff
                         then arbitrary
                         else return Mx.Mature
            let err_m = if state == Mx.Larval || state == Mx.Connected
                            then Just Mx.MuxUnknownMiniProtocol
                            else Nothing

            return $ ArbitraryValidSDU b state err_m

        tooLargeSdu = do
            l <- choose (1 + smallMiniProtocolLimit , 2 * smallMiniProtocolLimit)
            pl <- BL8.pack <$> replicateM (fromIntegral l) arbitrary

            -- This SDU is still considered valid, since the header itself will
            -- not cause a trouble, the error will be triggered by the fact that
            -- it is sent as a single message.
            return $ ArbitraryValidSDU (DummyPayload pl) Mx.Mature (Just Mx.MuxIngressQueueOverRun)

        unknownMiniProtocol = do
            ts  <- arbitrary
            mid <- choose (6, 0x7fff) -- ClientChainSynWithBlocks with 5 is the highest valid mid
            mode <- oneof [return 0x0, return 0x8000]
            len <- arbitrary
            state <- arbitrary
            p <- arbitrary

            return $ ArbitraryInvalidSDU (InvalidSDU (Mx.RemoteClockModel ts) (mid .|. mode) len
                                          (8 + fromIntegral len) p)
                                         state
                                         Mx.MuxUnknownMiniProtocol
        invalidLenght = do
            ts  <- arbitrary
            mid <- arbitrary
            len <- arbitrary
            realLen <- choose (0, 7) -- Size of mux header is 8
            p <- arbitrary
            state <- arbitrary

            return $ ArbitraryInvalidSDU (InvalidSDU (Mx.RemoteClockModel ts) mid len realLen p)
                                         state
                                         Mx.MuxDecodeError

instance Arbitrary Mx.MuxBearerState where
     -- XXX Larval and Connected test behaviour is dependant on version
     -- negotation so they are disabled for now.
     arbitrary = elements [ -- Mx.Larval
                            -- , Mx.Connected
                            Mx.Mature
                          , Mx.Dying
                          , Mx.Dead
                          ]



-- | A pair of two bytestrings which lengths are unevenly distributed
--
data Uneven = Uneven DummyPayload DummyPayload
  deriving (Eq, Show)

instance Arbitrary Uneven where
    arbitrary = do
      n <- choose (1, 128)
      b <- arbitrary
      (l, k) <- (if b then swap else id) <$>
                oneof [ (n,) <$> choose (2 * n, defaultMiniProtocolLimit - cborOverhead)
                      , let k = defaultMiniProtocolLimit - n - cborOverhead
                        in (k,) <$> choose (1, k `div` 2)
                      , do
                          k <- choose (1, defaultMiniProtocolLimit - cborOverhead)
                          return (k, k `div` 2)
                      ]
      Uneven <$> (DummyPayload <$> genLargeByteString 1024 l)
             <*> (DummyPayload <$> genLargeByteString 1024 k)
      where
        cborOverhead = 7 -- XXX Bytes needed by CBOR to encode the dummy payload


--
-- QuickChekc Properties
--


-- | Verify that an initiator and a responder can send and receive messages
-- from each other.  Large DummyPayloads will be split into sduLen sized
-- messages and the testcases will verify that they are correctly reassembled
-- into the original message.
--
prop_mux_snd_recv :: DummyTrace
                  -> Property
prop_mux_snd_recv messages = ioProperty $ do
    let sduLen = 1260

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    endMpsVar <- atomically $ newTVar 2

    let server_w = client_r
        server_r = client_w

    (verify, clientApp, serverApp) <- setupMiniReqRsp
                                        (return ()) endMpsVar messages

    clientAsync <-
      async $ Mx.runMuxWithQueues activeTracer "client" (Mx.MuxInitiatorApplication
        $ \_ ReqResp1 -> clientApp) client_w client_r sduLen Nothing
    serverAsync <-
      async $ Mx.runMuxWithQueues activeTracer "server" (Mx.MuxResponderApplication
        $ \_ ReqResp1 -> serverApp) server_w server_r sduLen Nothing

    r <- waitBoth clientAsync serverAsync
    case r of
         (Just _, _) -> return $ property False
         (_, Just _) -> return $ property False
         _           -> property <$> verify

-- | Create a verification function, a MiniProtocolDescription for the client
-- side and a MiniProtocolDescription for the server side for a RequestResponce
-- protocol.
--
setupMiniReqRsp :: IO ()              -- | Action performed by responder before processing the response
                -> StrictTVar IO Int  -- | Total number of miniprotocols.
                -> DummyTrace         -- | Trace of messages
                -> IO ( IO Bool
                      , Mx.Channel IO -> IO ()
                      , Mx.Channel IO -> IO ()
                      )
setupMiniReqRsp serverAction mpsEndVar (DummyTrace msgs) = do
    serverResultVar <- newEmptyTMVarM
    clientResultVar <- newEmptyTMVarM

    return ( verifyCallback serverResultVar clientResultVar
           , clientApp clientResultVar
           , serverApp serverResultVar
           )
  where
    requests  = map fst msgs
    responses = map snd msgs

    verifyCallback serverResultVar clientResultVar =
        atomically $ (&&) <$> takeTMVar serverResultVar <*> takeTMVar clientResultVar

    reqRespServer :: [DummyPayload]
                  -> ReqRespServer DummyPayload DummyPayload IO Bool
    reqRespServer = go []
      where
        go reqs (resp:resps) = ReqRespServer {
            recvMsgReq  = \req -> serverAction >> return (resp, go (req:reqs) resps),
            recvMsgDone = pure $ reverse reqs == requests
          }
        go reqs [] = ReqRespServer {
            recvMsgReq  = error "server out of replies",
            recvMsgDone = pure $ reverse reqs == requests
          }


    reqRespClient :: [DummyPayload]
                  -> ReqRespClient DummyPayload DummyPayload IO Bool
    reqRespClient = go []
      where
        go resps []         = SendMsgDone (pure $ reverse resps == responses)
        go resps (req:reqs) = SendMsgReq req $ \resp -> return (go (resp:resps) reqs)

    clientApp :: StrictTMVar IO Bool
              -> Mx.Channel IO
              -> IO ()
    clientApp clientResultVar clientChan = do
        result <- runClient (showTracing stdoutTracer) clientChan (reqRespClient requests)
        atomically (putTMVar clientResultVar result)
        end

    serverApp :: StrictTMVar IO Bool
              -> Mx.Channel IO
              -> IO ()
    serverApp serverResultVar serverChan = do
        result <- runServer (showTracing stdoutTracer) serverChan (reqRespServer responses)
        atomically (putTMVar serverResultVar result)
        end

    -- Wait on all miniprotocol jobs before letting a miniprotocol thread exit.
    end = do
        atomically $ modifyTVar mpsEndVar (\a -> a - 1)
        atomically $ do
            c <- readTVar mpsEndVar
            unless (c == 0) retry

waitOnAllClients :: StrictTVar IO Int
                 -> Int
                 -> IO ()
waitOnAllClients clientVar clientTot = do
        atomically $ modifyTVar clientVar (+ 1)
        atomically $ do
            c <- readTVar clientVar
            unless (c == clientTot) retry

--
-- Running with queues and pipes
--

type RunEnv ptcl
    = ( Mx.ProtocolEnum       ptcl
      , Mx.MiniProtocolLimits ptcl
      , Enum    ptcl
      , Bounded ptcl
      , Ord     ptcl
      , Show    ptcl
      )
    => IO ( Mx.MuxApplication Mx.InitiatorApp String ptcl IO () Void -> IO (Maybe SomeException)
          , Mx.MuxApplication Mx.ResponderApp String ptcl IO Void () -> IO (Maybe SomeException)
          )


runWithQueues :: RunEnv ptcl
runWithQueues = do
    let sduLen = 14000
    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    let server_w = client_r
        server_r = client_w
    pure $ ( \app ->
               Mx.runMuxWithQueues
                 (Mx.WithMuxBearer "client" `contramap` showTracing stdoutTracer)
                 "client" app client_w client_r sduLen Nothing
           , \app ->
               Mx.runMuxWithQueues
                 (Mx.WithMuxBearer "server" `contramap` showTracing stdoutTracer)
                 "server" app server_w server_r sduLen Nothing
           )


runWithPipe :: RunEnv ptcl
runWithPipe = do
#if defined(mingw32_HOST_OS)
    let pName = "\\\\.\\pipe\\mux-test-pipe"
    sync <- newEmptyTMVarM

    pure ( \app -> do
             atomically $ takeTMVar sync
             clientHANDLE <- Win32.createFile
               pName
               (Win32.File.gENERIC_READ .|. Win32.File.gENERIC_WRITE)
               Win32.File.fILE_SHARE_NONE
               Nothing
               Win32.File.oPEN_EXISTING
               Win32.File.fILE_ATTRIBUTE_NORMAL
               Nothing
             let clientChannel = Mx.pipeChannelFromHANDLE clientHANDLE
             res <- try $
                 Mx.runMuxWithPipes
                   (Mx.WithMuxBearer "client" `contramap` showTracing stdoutTracer)
                   "client" app clientChannel
               `finally`
                 Win32.closePipe clientHANDLE
             pure $ either Just (const Nothing) res

         , \app -> do
             serverHANDLE <- Win32.createNamedPipe
               pName
               Win32.pIPE_ACCESS_DUPLEX
               (Win32.pIPE_TYPE_BYTE .|. Win32.pIPE_READMODE_BYTE)
               Win32.pIPE_UNLIMITED_INSTANCES
               maxBound
               maxBound
               0
               Nothing
             atomically $ putTMVar sync ()
             Win32.connectNamedPipe serverHANDLE Nothing
             let serverChannel = Mx.pipeChannelFromHANDLE serverHANDLE
             res <- try $
                 Mx.runMuxWithPipes
                   (Mx.WithMuxBearer "server"`contramap` showTracing stdoutTracer)
                   "server" app serverChannel
              `finally`
                  Win32.closePipe serverHANDLE
             pure $ either Just (const Nothing) res
         )
#else
-- TODO
#endif

-- | Verify that it is possible to run two miniprotocols over the same bearer.
-- Makes sure that messages are delivered to the correct miniprotocol in order.
--
test_mux_1_mini :: RunEnv TestProtocols1
                -> DummyTrace
                -> IO Bool
test_mux_1_mini runEnv msgTrace = do
    (runClientApp, runServerApp) <- runEnv

    endMpsVar <- atomically $ newTVar 2

    (verify, client_mp, server_mp) <-
        setupMiniReqRsp (return ()) endMpsVar msgTrace

    let clientApp _ ReqResp1 = client_mp
        serverApp _ ReqResp1 = server_mp

    serverAsync <- async $ runServerApp (Mx.MuxResponderApplication serverApp)
    clientAsync <- async $ runClientApp (Mx.MuxInitiatorApplication clientApp)

    r <- waitBoth clientAsync serverAsync
    case r of
         (Just _, _) -> return False
         (_, Just _) -> return False
         _           -> verify

prop_mux_1_mini_Queue :: DummyTrace -> Property
prop_mux_1_mini_Queue = ioProperty . test_mux_1_mini runWithQueues

prop_mux_1_mini_Pipe :: DummyTrace -> Property
prop_mux_1_mini_Pipe = ioProperty . test_mux_1_mini runWithPipe

prop_1 :: Property
prop_1 = prop_mux_1_mini_Pipe
  (DummyTrace
    [ ( DummyPayload (BL8.pack "request")
      , DummyPayload (BL8.pack "response")
      )
    ])

-- | Verify that it is possible to run two miniprotocols over the same bearer.
-- Makes sure that messages are delivered to the correct miniprotocol in order.
--
test_mux_2_minis
    :: RunEnv TestProtocols2
    -> DummyTrace
    -> DummyTrace
    -> IO Bool
test_mux_2_minis runEnv msgTrace0 msgTrace1 = do
    (runClientApp, runServerApp) <- runEnv

    endMpsVar <- atomically $ newTVar 4 -- Two initiators and two responders.

    (verify_0, client_mp0, server_mp0) <-
        setupMiniReqRsp (return ()) endMpsVar msgTrace0
    (verify_1, client_mp1, server_mp1) <-
        setupMiniReqRsp (return ()) endMpsVar msgTrace1

    let clientApp _ ReqResp2 = client_mp0
        clientApp _ ReqResp3 = client_mp1

        serverApp _ ReqResp2 = server_mp0
        serverApp _ ReqResp3 = server_mp1

    serverAsync <- async $ runServerApp (Mx.MuxResponderApplication serverApp)
    clientAsync <- async $ runClientApp (Mx.MuxInitiatorApplication clientApp)

    r <- waitBoth clientAsync serverAsync
    case r of
         (Just _, _) -> return False
         (_, Just _) -> return False
         _           -> do
             res0 <- verify_0
             res1 <- verify_1

             return $ res0 && res1

prop_mux_2_minis_Queue :: DummyTrace
                       -> DummyTrace
                       -> Property
prop_mux_2_minis_Queue a b = ioProperty $ test_mux_2_minis runWithQueues a b

prop_mux_2_minis_Pipe :: DummyTrace
                      -> DummyTrace
                      -> Property
prop_mux_2_minis_Pipe a b = ioProperty $ test_mux_2_minis runWithPipe a b


-- | Attempt to verify that capacity is diveded fairly between two active
-- miniprotocols.  Two initiators send a request over two different
-- miniprotocols and the corresponding responders each send a large reply back.
-- The Mux bearer should alternate between sending data for the two responders.
--
prop_mux_starvation :: Uneven
                    -> Property
prop_mux_starvation (Uneven response0 response1) =
    let sduLen = 1260 in
    (BL.length (unDummyPayload response0) > 2 * fromIntegral sduLen) &&
    (BL.length (unDummyPayload response1) > 2 * fromIntegral sduLen) ==>
    ioProperty $ do
    let request       = DummyPayload $ BL.replicate 4 0xa

    client_w <- atomically $ newTBQueue 10
    client_r <- atomically $ newTBQueue 10
    activeMpsVar <- atomically $ newTVar 0
    -- 2 active initiators and 2 active responders
    endMpsVar <- atomically $ newTVar 4
    -- At most track 100 packets per test run
    traceQueueVar <- atomically $ newTBQueue 100

    let server_w = client_r
        server_r = client_w

    (verify_short, client_short, server_short) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                        endMpsVar  $ DummyTrace [(request, response0)]
    (verify_long, client_long, server_long) <-
        setupMiniReqRsp (waitOnAllClients activeMpsVar 2)
                        endMpsVar $ DummyTrace [(request, response1)]

    let clientApp _ ReqResp2 = client_short
        clientApp _ ReqResp3 = client_long

        serverApp _ ReqResp2 = server_short
        serverApp _ ReqResp3 = server_long

    clientAsync <- async $ Mx.runMuxWithQueues activeTracer "client" (Mx.MuxInitiatorApplication clientApp) client_w client_r sduLen (Just traceQueueVar)
    serverAsync <- async $ Mx.runMuxWithQueues activeTracer "server" (Mx.MuxResponderApplication serverApp) server_w server_r sduLen Nothing

    -- First verify that all messages where received correctly
    r <- waitBoth clientAsync serverAsync
    case r of
         (Just _, _) -> return $ property False
         (_, Just _) -> return $ property False
         _           -> do
             -- First verify that all messages where received correctly
             res_short <- verify_short
             res_long <- verify_long

             -- Then look at the message trace to check for starvation.
             trace <- atomically $ flushTBQueue traceQueueVar []
             let es = map (\(e, _, _) -> e) trace
                 ls = dropWhile (\e -> e == head es) es
                 fair = verifyStarvation ls
             return $ res_short .&&. res_long .&&. fair
  where
   -- We can't make 100% sure that both servers start responding at the same
   -- time but once they are both up and running messages should alternate
   -- between Mx.ReqResp2 and Mx.ReqResp3
    verifyStarvation :: [Mx.MiniProtocolId TestProtocols2] -> Property
    verifyStarvation [] = property True
    verifyStarvation ms =
      let ms' = dropWhileEnd (\e -> e == last ms)
                  (head ms : dropWhile (\e -> e == head ms) ms)
                ++ [last ms]
      in
        label ("length " ++ labelPr_ ((length ms' * 100) `div` length ms) ++ "%")
        $ label ("length " ++ label_ (length ms')) $ alternates ms'

      where
        alternates []     = True
        alternates (_:[]) = True
        alternates (a : b : as) = a /= b && alternates (b : as)

    label_ :: Int -> String
    label_ n = mconcat
      [ show $ n `div` 10 * 10
      , "-"
      , show $ n `div` 10 * 10 + 10 - 1
      ]

    labelPr_ :: Int -> String
    labelPr_ n | n >= 100  = "100"
               | otherwise = label_ n


    flushTBQueue q acc = do
        e <- isEmptyTBQueue q
        if e then return $ reverse acc
             else do
                 a <- readTBQueue q
                 flushTBQueue q (a : acc)

encodeInvalidMuxSDU :: InvalidSDU -> BL.ByteString
encodeInvalidMuxSDU sdu =
    let header = Bin.runPut enc in
    BL.append header $ BL.replicate (fromIntegral $ isLength sdu) (isPattern sdu)
  where
    enc = do
        Bin.putWord32be $ Mx.unRemoteClockModel $ isTimestamp sdu
        Bin.putWord16be $ isIdAndMode sdu
        Bin.putWord16be $ isLength sdu

-- | Verify ingress processing of valid and invalid SDUs.
--
prop_demux_sdu :: forall m.
                    ( MonadAsync m
                    , MonadCatch m
                    , MonadMask m
                    , MonadSay m
                    , MonadST m
                    , MonadSTM m
                    , MonadThrow (STM m)
                    , MonadTime m
                    , MonadTimer m
                    , Eq (Async m ())
                    )
                 => ArbitrarySDU
                 -> m Property
prop_demux_sdu a = do
    r <- run a
    return $ tabulate "SDU type" [stateLabel a] $
             tabulate "SDU Violation " [violationLabel a] r
  where
    run (ArbitraryValidSDU sdu state (Just Mx.MuxIngressQueueOverRun)) = do
        stopVar <- newEmptyTMVarM

        -- To trigger MuxIngressQueueOverRun we use a special test protocol
        -- with an ingress queue which is less than 0xffff so that it can be
        -- triggered by a single segment.
        let server_mps = Mx.MuxResponderApplication (\_ ReqRespSmall -> serverRsp stopVar)

        (client_w, said) <- plainServer server_mps
        setup state client_w

        writeSdu client_w $! unDummyPayload sdu

        atomically $! putTMVar stopVar $ unDummyPayload sdu

        res <- wait said
        case res of
            Just e  ->
                case fromException e of
                    Just me -> return $ Mx.errorType me === Mx.MuxIngressQueueOverRun
                    Nothing -> return $ property False
            Nothing -> return $ property False

    run (ArbitraryValidSDU sdu state err_m) = do
        stopVar <- newEmptyTMVarM

        let server_mps = Mx.MuxResponderApplication (\_ ReqResp1 -> serverRsp stopVar)

        (client_w, said) <- plainServer server_mps

        setup state client_w

        atomically $! putTMVar stopVar $! unDummyPayload sdu
        writeSdu client_w $ unDummyPayload sdu

        res <- wait said
        case res of
            Just e  ->
                case fromException e of
                    Just me -> case err_m of
                                    Just err -> return $ Mx.errorType me === err
                                    Nothing  -> return $ property False
                    Nothing -> return $ property False
            Nothing -> return $ err_m === Nothing

    run (ArbitraryInvalidSDU badSdu state err) = do
        stopVar <- newEmptyTMVarM

        let server_mps = Mx.MuxResponderApplication (\_ ReqResp1 -> serverRsp stopVar)

        (client_w, said) <- plainServer server_mps

        setup state client_w
        atomically $ writeTBQueue client_w $ BL.take (isRealLength badSdu) $ encodeInvalidMuxSDU badSdu
        atomically $ putTMVar stopVar $ BL.replicate (fromIntegral $ isLength badSdu) 0xa

        res <- wait said
        case res of
            Just e  ->
                case fromException e of
                    Just me -> return $ Mx.errorType me === err
                    Nothing -> return $ property False
            Nothing -> return $ property False

    plainServer server_mps = do
        server_w <- atomically $ newTBQueue 10
        server_r <- atomically $ newTBQueue 10

        said <- async $ Mx.runMuxWithQueues activeTracer "server" server_mps server_w server_r 1280 Nothing

        return (server_r, said)

    -- Server that expects to receive a specific ByteString.
    -- Doesn't send a reply.
    serverRsp stopVar chan =
        atomically (takeTMVar stopVar) >>= loop
      where
        loop e | e == BL.empty = return ()
        loop e = do
            msg_m <- Mx.recv chan
            case msg_m of
                 Just msg ->
                     case BL.stripPrefix msg e of
                          Just e' -> loop e'
                          Nothing -> error "recv corruption"
                 Nothing -> error "eof corruption"

    writeSdu _ payload | payload == BL.empty = return ()
    writeSdu queue payload = do
        let (!frag, !rest) = BL.splitAt 0xffff payload
            sdu' = Mx.MuxSDU (Mx.RemoteClockModel 0) (Mx.AppProtocolId ReqRespSmall)
                              Mx.ModeInitiator
                             (fromIntegral $ BL.length frag) frag
            !pkt = Mx.encodeMuxSDU (sdu' :: Mx.MuxSDU TestProtocolsSmall)

        atomically $ writeTBQueue queue pkt
        writeSdu queue rest

    -- Unless we are in Larval or Connected we fake version negotiation before
    -- we run the test.
    {- Not yet! setup state q | state /= Mx.Larval && state /= Mx.Connected = do
        let msg = Mx.MsgInitReq [version0]
            blob = toLazyByteString $ Mx.encodeControlMsg msg
            pkt = Mx.MuxSDU (Mx.RemoteClockModel 0) Mx.Muxcontrol Mx.ModeInitiator
                            (fromIntegral $ BL.length blob) blob
        atomically $ writeTBQueue q $ Mx.encodeMuxSDU (pkt :: Mx.MuxSDU TestProtocols1)
        return () -}
    setup _ _ = return ()

    stateLabel (ArbitraryInvalidSDU _ state _) = "Invalid " ++ versionLabel state
    stateLabel (ArbitraryValidSDU _ state _)   = "Valid " ++ versionLabel state

    versionLabel Mx.Larval    = "before version negotiation"
    versionLabel Mx.Connected = "before version negotiation"
    versionLabel _            = "after version negotiation"

    violationLabel (ArbitraryValidSDU _ _ err_m) = sduViolation err_m
    violationLabel (ArbitraryInvalidSDU _ _ err) = sduViolation $ Just err

    sduViolation (Just Mx.MuxUnknownMiniProtocol) = "unknown miniprotocol"
    sduViolation (Just Mx.MuxDecodeError        ) = "decode error"
    sduViolation (Just Mx.MuxIngressQueueOverRun) = "ingress queue overrun"
    sduViolation (Just _                        ) = "unknown violation"
    sduViolation Nothing                          = "none"

prop_demux_sdu_sim :: ArbitrarySDU
                     -> Property
prop_demux_sdu_sim badSdu =
    let r_e =  runSimStrictShutdown $ prop_demux_sdu badSdu in
    case r_e of
         Left  _ -> property False
         Right r -> r

prop_demux_sdu_io :: ArbitrarySDU
                    -> Property
prop_demux_sdu_io badSdu = ioProperty $ prop_demux_sdu badSdu
