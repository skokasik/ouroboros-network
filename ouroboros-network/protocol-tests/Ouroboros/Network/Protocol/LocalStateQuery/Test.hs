{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Network.Protocol.LocalStateQuery.Test (tests) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Data.ByteString.Lazy (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadST (MonadST)
import           Control.Monad.Class.MonadThrow (MonadCatch)
import           Control.Monad.IOSim
import           Control.Monad.ST (runST)
import           Control.Tracer (nullTracer)

import qualified Codec.Serialise as Serialise (decode, encode)

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Driver
import           Network.TypedProtocol.Proofs

import           Ouroboros.Network.Codec hiding (prop_codec)
import           Ouroboros.Network.Channel

import           Ouroboros.Network.MockChain.Chain (Point)
import           Ouroboros.Network.Testing.ConcreteBlock (Block)

import           Ouroboros.Network.Protocol.LocalStateQuery.Client
import           Ouroboros.Network.Protocol.LocalStateQuery.Codec
import           Ouroboros.Network.Protocol.LocalStateQuery.Direct
import           Ouroboros.Network.Protocol.LocalStateQuery.Examples
import           Ouroboros.Network.Protocol.LocalStateQuery.Server
import           Ouroboros.Network.Protocol.LocalStateQuery.Type

import           Test.ChainGenerators ()
import           Test.Ouroboros.Network.Testing.Utils (prop_codec_cborM,
                     splits2, splits3)

import           Test.QuickCheck as QC hiding (Result)
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Show.Functions ()


--
-- Test cases
--

tests :: TestTree
tests =
  testGroup "Ouroboros.Network.Protocol.LocalStateQuery"
  [ testProperty "direct"              prop_direct
  , testProperty "connect"             prop_connect
  , testProperty "codec"               prop_codec
  , testProperty "codec 2-splits"      prop_codec_splits2
  , testProperty "codec 3-splits"    $ withMaxSuccess 30
                                       prop_codec_splits3
  , testProperty "codec cbor"          prop_codec_cbor
  , testProperty "channel ST"          prop_channel_ST
  , testProperty "channel IO"          prop_channel_IO
  , testProperty "pipe IO"             prop_pipe_IO
  ]


--
-- Common types & clients and servers used in the tests in this module.
--

data Query result where
  QueryPoint :: Query (Point Block)

deriving instance Show (Query result)

-- | Information to test an example server and client.
data Setup = Setup
  { clientInput   :: [(Point Block, Query (Point Block))]
    -- ^ Input for 'localStateQueryClient'
  , serverAcquire :: Point Block -> Either AcquireFailure (Point Block)
    -- ^ First input parameter for 'localStateQueryServer'
  , serverAnswer  :: forall result. Point Block -> Query result -> result
    -- ^ Second input parameter for 'localStateQueryServer'
  , expected      :: [(Point Block, Either AcquireFailure (Point Block))]
    -- ^ Expected result for the 'localStateQueryClient'.
  }

mkSetup
  :: Map (Point Block) (Maybe AcquireFailure, Query (Point Block))
     -- ^ For each point, the given state queries will be executed. In case of
     -- the second field is an 'AcquireFailure', the server will fail with
     -- that failure.
     --
     -- This is the randomly generated input for the 'Setup'.
  -> Setup
mkSetup input = Setup {
      clientInput   = [(pt, q) | (pt, (_, q)) <- Map.toList input]
    , serverAcquire = \pt -> case Map.lookup pt input of
        Just (Just failure, _qs) -> Left failure
        Just (Nothing,      _qs) -> Right pt
        Nothing                  -> error $
          "a point not in the input was tried to be acquired: " <> show pt
    , serverAnswer  = answer
    , expected      =
        [ (pt, res)
        | (pt, (mbFailure, q)) <- Map.toList input
        , let res = case mbFailure of
                Nothing      -> Right $ answer pt q
                Just failure -> Left failure
        ]
    }
  where
    answer :: Point Block -> Query result -> result
    answer pt q = case q of
      QueryPoint -> pt


--
-- Properties going directly, not via Peer.
--

-- | Run a simple local state query client and server, directly on the wrappers,
-- without going via the 'Peer'.
--
prop_direct :: Map (Point Block) (Maybe AcquireFailure, Query (Point Block))
            -> Property
prop_direct input =
    runSimOrThrow
      (direct
        (localStateQueryClient clientInput)
        (localStateQueryServer serverAcquire serverAnswer))
  ===
    (expected, ())
  where
    Setup { clientInput, serverAcquire, serverAnswer, expected } = mkSetup input


--
-- Properties going via Peer, but without using a channel
--

-- | Run a simple local state query client and server, going via the 'Peer'
-- representation, but without going via a channel.
--
prop_connect :: Map (Point Block) (Maybe AcquireFailure, Query (Point Block))
             -> Property
prop_connect input =
    case runSimOrThrow
           (connect
             (localStateQueryClientPeer $
              localStateQueryClient clientInput)
             (localStateQueryServerPeer $
              localStateQueryServer serverAcquire serverAnswer)) of

      (result, (), TerminalStates TokDone TokDone) -> result === expected
  where
    Setup { clientInput, serverAcquire, serverAnswer, expected } = mkSetup input


--
-- Properties using a channel
--

-- | Run a local state query client and server using connected channels.
--
prop_channel :: (MonadAsync m, MonadCatch m, MonadST m)
             => m (Channel m ByteString, Channel m ByteString)
             -> Map (Point Block) (Maybe AcquireFailure, Query (Point Block))
             -> m Property
prop_channel createChannels input =

    ((expected, ()) ===) <$>

    runConnectedPeers
      createChannels
      nullTracer
      codec
      (localStateQueryClientPeer $
       localStateQueryClient clientInput)
      (localStateQueryServerPeer $
       localStateQueryServer serverAcquire serverAnswer)
  where
    Setup { clientInput, serverAcquire, serverAnswer, expected } = mkSetup input

-- | Run 'prop_channel' in the simulation monad.
--
prop_channel_ST :: Map (Point Block) (Maybe AcquireFailure, Query (Point Block))
                -> Property
prop_channel_ST input =
    runSimOrThrow
      (prop_channel createConnectedChannels input)

-- | Run 'prop_channel' in the IO monad.
--
prop_channel_IO :: Map (Point Block) (Maybe AcquireFailure, Query (Point Block))
                -> Property
prop_channel_IO input =
    ioProperty (prop_channel createConnectedChannels input)

-- | Run 'prop_channel' in the IO monad using local pipes.
--
prop_pipe_IO :: Map (Point Block) (Maybe AcquireFailure, Query (Point Block))
             -> Property
prop_pipe_IO input =
    ioProperty (prop_channel createPipeConnectedChannels input)


--
-- Codec properties
--

instance Arbitrary AcquireFailure where
  arbitrary = elements
    [ AcquireFailurePointTooOld
    , AcquireFailurePointNotOnChain
    ]

instance Arbitrary (Query (Point Block)) where
  arbitrary = pure QueryPoint

instance Arbitrary (AnyMessageAndAgency (LocalStateQuery Block Query)) where
  arbitrary = oneof
    [ AnyMessageAndAgency (ClientAgency TokIdle) <$>
        (MsgAcquire <$> arbitrary)

    , AnyMessageAndAgency (ServerAgency TokAcquiring) <$>
        pure MsgAcquired

    , AnyMessageAndAgency (ServerAgency TokAcquiring) <$>
        (MsgFailure <$> arbitrary)

    , AnyMessageAndAgency (ClientAgency TokAcquired) <$>
        (MsgQuery <$> (arbitrary :: Gen (Query (Point Block))))

    , AnyMessageAndAgency (ServerAgency (TokQuerying QueryPoint)) <$>
        (MsgResult QueryPoint <$> arbitrary)

    , AnyMessageAndAgency (ClientAgency TokAcquired) <$>
        pure MsgRelease

    , AnyMessageAndAgency (ClientAgency TokAcquired) <$>
        (MsgReAcquire <$> arbitrary)

    , AnyMessageAndAgency (ClientAgency TokIdle) <$>
        pure MsgDone
    ]

instance ShowQuery Query where
  showResult QueryPoint = show

instance Show (AnyMessageAndAgency (LocalStateQuery Block Query)) where
  show (AnyMessageAndAgency _ msg) = show msg

instance  Eq (AnyMessage (LocalStateQuery Block Query)) where

  (==) (AnyMessage (MsgAcquire pt))
       (AnyMessage (MsgAcquire pt')) = pt == pt'

  (==) (AnyMessage MsgAcquired)
       (AnyMessage MsgAcquired) = True

  (==) (AnyMessage (MsgFailure failure))
       (AnyMessage (MsgFailure failure')) = failure == failure'

  (==) (AnyMessage (MsgQuery query))
       (AnyMessage (MsgQuery query')) =
         case (query, query') of
           (QueryPoint, QueryPoint) -> True

  (==) (AnyMessage (MsgResult query  result))
       (AnyMessage (MsgResult query' result')) =
         case (query, query') of
           (QueryPoint, QueryPoint) -> result == result'

  (==) (AnyMessage MsgRelease)
       (AnyMessage MsgRelease) = True

  (==) (AnyMessage (MsgReAcquire pt))
       (AnyMessage (MsgReAcquire pt')) = pt == pt'

  (==) (AnyMessage MsgDone)
       (AnyMessage MsgDone) = True

  _ == _ = False


codec :: MonadST m
      => Codec (LocalStateQuery Block Query)
                DeserialiseFailure
                m ByteString
codec = codecLocalStateQuery
          Serialise.encode Serialise.decode
          encodeQuery      decodeQuery
          encodeResult     decodeResult
  where
    encodeQuery :: Query result -> CBOR.Encoding
    encodeQuery QueryPoint = Serialise.encode ()

    decodeQuery :: forall s . CBOR.Decoder s (Some Query)
    decodeQuery = do
      () <- Serialise.decode
      return $ Some QueryPoint

    encodeResult :: Query result -> result -> CBOR.Encoding
    encodeResult QueryPoint = Serialise.encode

    decodeResult :: Query result -> forall s. CBOR.Decoder s result
    decodeResult QueryPoint = Serialise.decode

-- | Check the codec round trip property.
--
prop_codec :: AnyMessageAndAgency (LocalStateQuery Block Query) -> Bool
prop_codec msg =
  runST (prop_codecM codec msg)

-- | Check for data chunk boundary problems in the codec using 2 chunks.
--
prop_codec_splits2 :: AnyMessageAndAgency (LocalStateQuery Block Query) -> Bool
prop_codec_splits2 msg =
  runST (prop_codec_splitsM splits2 codec msg)

-- | Check for data chunk boundary problems in the codec using 3 chunks.
--
prop_codec_splits3 :: AnyMessageAndAgency (LocalStateQuery Block Query) -> Bool
prop_codec_splits3 msg =
  runST (prop_codec_splitsM splits3 codec msg)

prop_codec_cbor
  :: AnyMessageAndAgency (LocalStateQuery Block Query)
  -> Bool
prop_codec_cbor msg =
  runST (prop_codec_cborM codec msg)
