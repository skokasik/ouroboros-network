{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Dynamic.Util.NetPartitionPlan
  ( -- * Network Partition Plan
    NetPartitionPlan (..)
  , genNetPartitionPlan
  , shrinkNetPartitionPlan
  , activeNetPartitionPlan
  , assignmentNetPartitionPlan
  , joinSlotsNetPartitionPlan
    -- * Network Class Identifier
  , NetClassId   -- opaque
  , NetClasses   -- opaque
  , tabulateNetClasses
  , lookupNetClasses
  , apNetClasses
  , foldNetClasses
  , onNetPartition
  , max2NetClasses
    -- * Refined Plans
  , RefinedNetPartitionPlan   -- opaque
  , refineNetPartitionPlan
  , getRefinedNetPartitionPlan
  ) where

import           Data.Function (on)
import qualified Data.Graph as Graph
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)
import           Test.QuickCheck

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Test.Dynamic.Util.NodeJoinPlan
import           Test.Dynamic.Util.NodeTopology

{-------------------------------------------------------------------------------
  Network Partition Plan
-------------------------------------------------------------------------------}

-- | A division of the core nodes into two classes and a scheduled interval of
-- slots
--
data NetPartitionPlan = NetPartitionPlan
    { nppMinor    :: !(Set.Set CoreNodeId)
    -- ^ The smaller of the two classes, favoring the one with node 0 in case
    -- of tie
    --
    -- INVARIANT: neither of the two classes is empty
    , nppInterval :: !(SlotNo, SlotNo)
    -- ^ The interval during which the nodes are partitioned
    --
    -- INVARIANT @first <= final@
    }
  deriving (Eq, Show)

instance Condense NetPartitionPlan where
  condense (NetPartitionPlan x (a, b)) = "NetPartitionPlan " <> condense (x, a, b)

-- | Generate a 'NetPartitionPlan' consistent with the given properties
--
genNetPartitionPlan ::
     HasCallStack
  => NumCoreNodes
     -- ^ PRECONDITION: non-negative
  -> NumSlots
     -- ^ PRECONDITION: positive
  -> Gen NetPartitionPlan
genNetPartitionPlan numCoreNodes@(NumCoreNodes n) numSlots@(NumSlots t)
  | n < 0 || t < 1 = error $ "Invalid inputs: "
    ++ show (numCoreNodes, numSlots)

  | otherwise = do
    let nids = enumCoreNodes numCoreNodes :: [CoreNodeId]

    let bad s = Set.null s || n == Set.size s   -- empty or full
    part1 <- (Set.fromList <$> sublistOf nids) `suchThat` (not . bad)
    let part2 = Set.fromList nids `Set.difference` part1
    let minor = case Ord.comparing Set.size part1 part2 of
          LT -> part1
          EQ
            | CoreNodeId 0 `Set.member` part1 -> part1
            | otherwise                       -> part2
          GT -> part2

    let lastSlot = t - 1
    begin <- choose (0,     lastSlot)
    end   <- choose (begin, lastSlot)

    let mkSlot = SlotNo . toEnum
    pure NetPartitionPlan
      { nppMinor    = minor
      , nppInterval = (mkSlot begin, mkSlot end)
      }

-- | Shrink a network partition plan
--
-- The new plans must be usable with the same number of nodes and slots as the
-- old plan
--
shrinkNetPartitionPlan :: NetPartitionPlan -> [NetPartitionPlan]
shrinkNetPartitionPlan = const []   -- TODO richer shrinking

newtype NetClassId = NetClassId Bool
  deriving (Eq, Show)

instance Condense NetClassId where
  condense = show

-- | Assign a node to a class
--
assignmentNetPartitionPlan :: NetPartitionPlan -> CoreNodeId -> NetClassId
assignmentNetPartitionPlan npp nid =
    NetClassId $ Set.member nid (nppMinor npp)

-- | 'EQ' if the partition is active, 'LT' if it hasn't started, 'GT' if it's over
--
activeNetPartitionPlan :: NetPartitionPlan -> SlotNo -> Ordering
activeNetPartitionPlan npp slot
  | slot < fst i = LT
  | slot > snd i = GT
  | otherwise = EQ
  where
    i = nppInterval npp

-- | The nodes in each class
--
-- It's easier to use 'assignmentNetPartitionPlan', if possible.
--
classesNetPartitionPlan ::
     NumCoreNodes
  -> NetPartitionPlan
  -> NetClasses (Set.Set CoreNodeId)
classesNetPartitionPlan numCoreNodes npp = tabulateNetClasses $
    \(NetClassId b) -> if b then minor else major
  where
    universe :: Set.Set CoreNodeId
    universe = Set.fromList $ enumCoreNodes numCoreNodes

    minor = nppMinor npp
    major = universe `Set.difference` minor

-- | The earliest join slot of a node in each class
--
joinSlotsNetPartitionPlan ::
     HasCallStack
  => NodeJoinPlan
  -> NetPartitionPlan
  -> NetClasses SlotNo
joinSlotsNetPartitionPlan njp npp =
    fmap f parts
  where
    f part = case Set.toList part of
      [] -> error "invalid NetPartitionPlan"
      o  -> minimum $ map (coreNodeIdJoinSlot njp) o

    parts = classesNetPartitionPlan (numCoreNodesNodeJoinPlan njp) npp

{-------------------------------------------------------------------------------
  Network Partition Class Identifier
-------------------------------------------------------------------------------}

data NetClasses a = NetClasses !a !a
  deriving (Eq, Foldable, Functor, Show, Traversable)

tabulateNetClasses :: (NetClassId -> a) -> NetClasses a
tabulateNetClasses f =
    (f . NetClassId) <$> NetClasses False True

lookupNetClasses :: NetClasses a -> NetClassId -> a
lookupNetClasses (NetClasses fls tru) (NetClassId b) =
    if b then tru else fls

apNetClasses ::
     NetClasses (a -> b)
  -> NetClasses a
  -> NetClasses b
apNetClasses
  (NetClasses lf rf) (NetClasses la ra) =
    NetClasses (lf la) (rf ra)

foldNetClasses ::
     (a -> a -> a)
  -> NetClasses a
  -> a
foldNetClasses f (NetClasses l r) = f l r

onNetPartition ::
     NetClassId
  -> (a -> a)
  -> NetClasses a
  -> NetClasses a
onNetPartition k f = apNetClasses $
    (\b -> if b then f else id) <$> tabulateNetClasses (== k)

-- | The two greatest values @(a, b)@, where @a <= b@
--
max2NetClasses :: Ord a => NetClasses a -> (a, a)
max2NetClasses (NetClasses a b) =
    if a > b then (b, a) else (a, b)

{-------------------------------------------------------------------------------
  Partition Plan Refinement
-------------------------------------------------------------------------------}

-- | An opaque type used to ensure the 'NetPartitionPlan' is already refined
--
-- INVARIANT: @'refineNetPartitionPlan . 'getRefinedNetPartitionPlan' =
-- 'getRefinedNetPartitionPlan'@
--
newtype RefinedNetPartitionPlan = Unsafe_RefinedNetPartitionPlan
  { getRefinedNetPartitionPlan :: NetPartitionPlan
  }
  deriving (Eq, Show)

instance Condense RefinedNetPartitionPlan where
  condense = ("Refined" <>) . condense . getRefinedNetPartitionPlan

-- | Use the other plans in the test configuration to refine the partition
--
-- This function attempts to delay the beginning of the network partition to
-- satisfy the following invariant. If unable, it returns 'Nothing', discarding
-- the network as nonsensical or over-complicated.
--
-- INVARIANT: At all times during the partition, its classes of nodes are
-- one-to-one with non-empty connected components of the current topology of
-- joined nodes.
--
-- COROLLARY: At all times during the partition, each joined node in a class is
-- not reachable by any nodes not in that same class. Otherwise that class
-- would not actually be separated from other classes.
--
-- COROLLARY: At all times during the partition, each joined node in a class is
-- reachable by every other joined node in that same class. Otherwise there
-- would actually be (sub)classes within that class.
--
-- A partition may be additionally divisive in the presence of a non-mesh
-- 'NodeTopology'. For example, with a linear topology @c0 <-> c1 <-> c2@, a
-- partition into @{c1}@ and @{c0, c2}@ actually creates three one-node
-- degerate connected components, since the topology doesn't connect @c0@ and
-- @c2@. More than two classes is more complicated of a test than we yet
-- desire, and the rest of this module only supports exactly two classes as of
-- yet.
--
-- The 'NodeTopology' specifies the final topology once all nodes join
-- according to the 'NodeJoinPlan' schedule. Until then, the topology of the
-- joined nodes approaches that final one as each node joins such that each
-- topology is a /vertex-induced subgraph/ of the next (i.e. nothing is
-- removed, and each new edge must involve at least one new node).
--
-- It's tempting to think it would be sufficient for a single class to consist
-- of multiple connected components as a long as a node connected to all of
-- those components joined that class soon enough. But \"soon enough\" is
-- difficult to characterize precisely and, in fact, induces the bulk of the
-- complexity in "Test.Dynamic.Util.Expectations", which can only be leveraged
-- if the above invariant always holds.
--
-- Even with the trivial mesh topology, a partition can't begin before a node
-- from each class has joined according to the 'NodeJoinPlan': the classes must
-- always be non-empty when restricted to nodes that have already joined. For
-- example, if the minor class contains only one node and the partition
-- interval ends before that node even joins, then there's actually no
-- partition, so this function would return @Nothing@.
--
refineNetPartitionPlan ::
     NodeJoinPlan
  -> NodeTopology
  -> NetPartitionPlan
  -> Maybe RefinedNetPartitionPlan
refineNetPartitionPlan njp ntop npp =
    fmap Unsafe_RefinedNetPartitionPlan $
    fmap (\i' -> npp{nppInterval = i'}) $
    earliestLongestRun ok [fst i .. snd i]
  where
    i = nppInterval npp

    ok :: SlotNo -> Bool
    ok slot = 2 == length (Graph.components gr)
      where
        (gr, _, _) = Graph.graphFromEdges
            [ (nid, nid, filter (sameClass nid) lessers)
            | (nid, lessers) <- m'
            ]

        m' =
            takeWhile ((>= slot) . joinSlot . fst) $
            Map.toAscList m
        NodeTopology m = ntop
        sameClass = (==) `on` assignmentNetPartitionPlan npp
        joinSlot  = coreNodeIdJoinSlot njp

earliestLongestRun :: (a -> Bool) -> [a] -> Maybe (a, a)
earliestLongestRun f = go Nothing Nothing
  where
    go !s0 !s1 = \case
      []            -> (\(_, a, b) -> (a, b)) <$> best s0 s1
      x:xs
        | f x       -> go s0           (Just $! extend s1 x) xs
        | otherwise -> go (best s0 s1) Nothing               xs

    extend Nothing          x = (0 :: Int, x, x)
    extend (Just (i, a, _)) x = let i' = i + 1 in i' `seq` (i, a, x)

    best Nothing            s                  = s
    best s                  Nothing            = s
    best (Just l@(i, _, _)) (Just r@(j, _, _)) =
        Just $ if j > i then r else l
