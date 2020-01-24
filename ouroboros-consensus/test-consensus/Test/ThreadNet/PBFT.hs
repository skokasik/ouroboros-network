{-# LANGUAGE NamedFieldPuns #-}

module Test.ThreadNet.PBFT (
    tests
  ) where

import qualified Data.Map as Map
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.BlockchainTime.Mock
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..))
import           Ouroboros.Consensus.Ledger.Mock.Block
import           Ouroboros.Consensus.Ledger.Mock.Block.PBFT
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol
import           Ouroboros.Consensus.Util.Random (Seed (..))

import           Test.ThreadNet.General
import           Test.ThreadNet.Util
import           Test.ThreadNet.Util.NodeJoinPlan
import           Test.ThreadNet.Util.NodeRestarts
import           Test.ThreadNet.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "PBFT" [
      testProperty "simple convergence" $
        prop_simple_pbft_convergence k $
           -- This is a Nondet plan: the pbftLimit is 1 block and c3 would
           -- choose among the three 1-block chains when it leads in slot 28,
           -- but c2 raises a MockInvalidInputs error (during
           -- 'reapplyTxSameState') in slot 26.
           let ncn5 = NumCoreNodes 5 in
           TestConfig
             { numCoreNodes = ncn5
             , numSlots     = NumSlots 100
             , nodeJoinPlan = NodeJoinPlan $ Map.fromList
               [ (CoreNodeId 0, SlotNo 0)   -- 0 only leads this slot
               , (CoreNodeId 1, SlotNo 6)   -- 1 only leads this slot
               , (CoreNodeId 2, SlotNo 22)  -- 2 only leads this slot
               , (CoreNodeId 3, SlotNo 24)
               , (CoreNodeId 4, SlotNo 99)  -- irrelevant, beyond affecting pbftThreshold
               ]
             , nodeRestarts = noRestarts
             , nodeTopology = meshNodeTopology ncn5
             , slotLengths  = singletonSlotLengths $ slotLengthFromSec 1
             , initSeed     = Seed (9550173506264790139,4734409083700350196,9697926137031612922,16476814117921936461,9569412668768792610)
             }
    ]
  where
    k = SecurityParam 5

prop_simple_pbft_convergence :: SecurityParam
                             -> TestConfig
                             -> Property
prop_simple_pbft_convergence
  k testConfig@TestConfig{numCoreNodes, numSlots} =
    prop_general k
        testConfig
        (Just $ roundRobinLeaderSchedule numCoreNodes numSlots)
        (expectedBlockRejection numCoreNodes)
        testOutput
  where
    NumCoreNodes nn = numCoreNodes

    sigThd = (1.0 / fromIntegral nn) + 0.1
    params = PBftParams k numCoreNodes sigThd (slotLengthFromSec 20)

    testOutput =
        runTestNetwork testConfig TestConfigBlock
            { forgeEBB = Nothing
            , nodeInfo = \nid -> protocolInfo $ ProtocolMockPBFT params nid
            , rekeying = Nothing
            }

type Blk = SimpleBlock SimpleMockCrypto
             (SimplePBftExt SimpleMockCrypto PBftMockCrypto)

expectedBlockRejection :: NumCoreNodes -> BlockRejection Blk -> Bool
expectedBlockRejection (NumCoreNodes nn) BlockRejection
  { brBlockSlot = SlotNo s
  , brReason    = err
  , brRejector  = CoreId (CoreNodeId i)
  }
  | ownBlock               = case err of
    ExtValidationErrorOuroboros
      PBftExceededSignThreshold{} -> True
    _                             -> False
  where
    -- Because of round-robin and the fact that the id divides slot, we know
    -- the node lead but rejected its own block. This is the only case we
    -- expect. (Rejecting its own block also prevents the node from propagating
    -- that block.)
    ownBlock = fromIntegral i == mod s (fromIntegral nn)
expectedBlockRejection _ _ = False
