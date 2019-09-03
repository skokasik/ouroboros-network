module Test.Dynamic.Util.Tests (
    tests
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Consensus.BlockchainTime (NumSlots (..), SlotNo (..))
import           Ouroboros.Consensus.Demo (defaultSecurityParam)
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
                     (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))

import           Test.Dynamic.Util
import           Test.Dynamic.Util.NetPartitionPlan
import           Test.Dynamic.Util.NodeJoinPlan
import           Test.Dynamic.Util.NodeTopology

import           Test.Util.Orphans.Arbitrary ()

tests :: TestTree
tests = testGroup "Test.Dynamic.Util.Tests" $
    [ motivatingMeshPartitionTests
    , motivatingNonMeshPartitionTests
    , testProperty "0 = determineForkLength roundRobinLeaderSchedule" $
          prop_roundRobin_forkLength defaultSecurityParam
    ]

-- | A round-robin schedule should reach consensus
prop_roundRobin_forkLength ::
    SecurityParam -> NumCoreNodes -> NumSlots -> Property
prop_roundRobin_forkLength k numCoreNodes numSlots =
  determineForkLength k nodeJoinPlan Nothing schedule === NumBlocks 0
  where
    nodeJoinPlan = trivialNodeJoinPlan numCoreNodes
    schedule = roundRobinLeaderSchedule numCoreNodes numSlots


-- | Cases with mesh topology that motivate expectations involving network
-- partitions
motivatingMeshPartitionTests :: TestTree
motivatingMeshPartitionTests =
    testGroup "motivatingMeshPartitionTests" $
    zipWith
      (\i prop ->
         localOption (QuickCheckTests 1) $
         testProperty ("Case " <> show (i :: Int)) $ prop
      ) [1 ..] $
    [ mk (NumSlots 2)
         (NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 1}),(CoreNodeId 2,SlotNo {unSlotNo = 1})]))
         (NetPartitionPlan {nppMinor = Set.fromList [CoreNodeId 2], nppInterval = (SlotNo {unSlotNo = 1},SlotNo {unSlotNo = 1})})
    , mk (NumSlots 3)
         (NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 2}),(CoreNodeId 2,SlotNo {unSlotNo = 2})]))
         (NetPartitionPlan {nppMinor = Set.fromList [CoreNodeId 2], nppInterval = (SlotNo {unSlotNo = 2},SlotNo {unSlotNo = 2})})
    , mk (NumSlots 4)
         (NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 1}),(CoreNodeId 1,SlotNo {unSlotNo = 1}),(CoreNodeId 2,SlotNo {unSlotNo = 3}),(CoreNodeId 3,SlotNo {unSlotNo = 3})]))
         (NetPartitionPlan {nppMinor = Set.fromList [CoreNodeId 1], nppInterval = (SlotNo {unSlotNo = 3},SlotNo {unSlotNo = 3})})
    , mk (NumSlots 19)
         (NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 0})]))
         (NetPartitionPlan {nppMinor = Set.fromList [CoreNodeId 0], nppInterval = (SlotNo {unSlotNo = 6},SlotNo {unSlotNo = 17})})
    , mk (NumSlots 20)
         (NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 0}),(CoreNodeId 2,SlotNo {unSlotNo = 10})]))
         (NetPartitionPlan {nppMinor = Set.fromList [CoreNodeId 2], nppInterval = (SlotNo {unSlotNo = 10},SlotNo {unSlotNo = 19})})
    , mk (NumSlots 26)
         (NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 15})]))
         (NetPartitionPlan {nppMinor = Set.fromList [CoreNodeId 0], nppInterval = (SlotNo {unSlotNo = 15},SlotNo {unSlotNo = 25})})
    ]
  where
    mk _ns njp npp =
        -- _ns is relevant if a developer laters want to run the test
        let ncn = numCoreNodesNodeJoinPlan njp
        in
        Nothing === refineNetPartitionPlan njp (meshNodeTopology ncn) npp

-- | Cases with non-mesh topology that motivate expectations involving network
-- partitions
motivatingNonMeshPartitionTests :: TestTree
motivatingNonMeshPartitionTests =
    testGroup "motivatingNonMeshPartitionTests" $
    zipWith
      (\i prop ->
         localOption (QuickCheckTests 1) $
         testProperty ("Case " <> show (i :: Int)) $ prop
      ) [1 ..] $
    [ mk (NumSlots 5)
         (NodeJoinPlan (Map.fromList [(CoreNodeId 0,SlotNo {unSlotNo = 0}),(CoreNodeId 1,SlotNo {unSlotNo = 0}),(CoreNodeId 2,SlotNo {unSlotNo = 0}),(CoreNodeId 3,SlotNo {unSlotNo = 3}),(CoreNodeId 4,SlotNo {unSlotNo = 4})]))
         (NodeTopology (Map.fromList [(CoreNodeId 0,[]),(CoreNodeId 1,[CoreNodeId 0]),(CoreNodeId 2,[CoreNodeId 0]),(CoreNodeId 3,[CoreNodeId 1]),(CoreNodeId 4,[CoreNodeId 0,CoreNodeId 1,CoreNodeId 3])]))
         (NetPartitionPlan {nppMinor = Set.fromList [CoreNodeId 0,CoreNodeId 4], nppInterval = (SlotNo {unSlotNo = 1},SlotNo {unSlotNo = 3})})
    ]
  where
    mk _ns njp ntop npp =
        -- _ns is relevant if a developer laters want to run the test
        Nothing === refineNetPartitionPlan njp ntop npp
