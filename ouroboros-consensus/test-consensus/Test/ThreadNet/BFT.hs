{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Test.ThreadNet.BFT (
    tests
  ) where

import qualified Data.Map.Strict as Map

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block (HasHeader, SlotNo (..), blockSlot)

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Consensus.Util.Random (Seed (..))

import           Test.ThreadNet.General
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Consensus.BlockchainTime.SlotLengths ()
import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "BFT" $
    [ testProperty "Mock.applyChainTick is not a no-op" $
        -- This repro failed via 'prop_valid_block' with a cherry-picked fix
        -- for Issue #1489. PR #1562 fixed it; it's passing locally with the
        -- same cherry-picked #1489 fix.
        --
        -- TODO Update this whole comment as part of the PR for Issue #1489.
        once $
        prop_simple_bft_convergence k5
        TestConfig
          { numCoreNodes = ncn3
          , numSlots     = NumSlots 7
          , nodeJoinPlan = NodeJoinPlan $ Map.fromList [(CoreNodeId 0, SlotNo 0),(CoreNodeId 1, SlotNo 2),(CoreNodeId 2, SlotNo 2)]
          , nodeRestarts = noRestarts
          , nodeTopology = meshNodeTopology ncn3
          , slotLengths  = defaultSlotLengths
          , initSeed     = Seed (6358650144370660550,17563794202468751585,17692838336641672274,12649320068211251815,18441126729279419067)
          }
    , testProperty "simple convergence" $
        prop_simple_bft_convergence k5
    ]
  where
    ncn3               = NumCoreNodes 3
    k5                 = SecurityParam 5
    defaultSlotLengths = singletonSlotLengths $ slotLengthFromSec 1

prop_simple_bft_convergence :: SecurityParam
                            -> TestConfig
                            -> Property
prop_simple_bft_convergence k
  testConfig@TestConfig{numCoreNodes, numSlots, slotLengths} =
    tabulate "slot length changes" [show $ countSlotLengthChanges numSlots slotLengths] $
    prop_general
        countSimpleGenTxs
        k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        (const False)
        prop_valid_block
        testOutput
  where
    testOutput =
        runTestNetwork testConfig TestConfigBlock
            { forgeEBB = Nothing
            , nodeInfo = \nid -> protocolInfo $
                ProtocolMockBFT numCoreNodes nid k slotLengths
            , rekeying = Nothing
            }

prop_valid_block
  :: HasHeader (SimpleBlock' c ext ext')
  => SimpleBlock' c ext ext' -> Property
prop_valid_block blk = conjoin $ map each $ simpleTxs $ simpleBody blk
  where
    now :: SlotNo
    now = blockSlot blk

    msg :: String
    msg = "block contains expired transaction:"

    each :: Tx -> Property
    each tx@(Tx expiry _ins _outs) =
      counterexample (msg <> " " <> condense (now, tx)) $
      case expiry of
        DoNotExpire       -> True
        ExpireAtOnsetOf s -> now < s
