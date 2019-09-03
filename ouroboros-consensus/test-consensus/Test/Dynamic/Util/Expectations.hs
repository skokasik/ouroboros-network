{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Test.Dynamic.Util.Expectations
    ( NumBlocks (..)
    , determineForkLength
    ) where

import           Data.Foldable (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Ord as Ord
import           Data.Word (Word64)

import           Ouroboros.Consensus.BlockchainTime (SlotNo (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))
import           Ouroboros.Consensus.Protocol.LeaderSchedule
                     (LeaderSchedule (..))

import           Test.Dynamic.Util.NetPartitionPlan
import           Test.Dynamic.Util.NodeJoinPlan

newtype NumBlocks = NumBlocks Word64
  deriving (Eq, Show)

-- | Internal accumulator of 'determineForkLength', per class
data Acc = Acc
  { maxChainLength :: !Word64
    -- ^ Upper bound on length of the longest chain in the class
  , maxForkDepth   :: !Word64
    -- ^ Upper bound on depth of the deepest disagreement between two chains in
    -- the class
    --
    -- When greater than @k@, it may be the deepest disagreement between any
    -- two chains. Otherwise, it's the deepest disagreement between two chains
    -- that both have the maximum length (it's 0 if only one such chain
    -- exists).
    --
    -- Note that @0@ corresponds to /consensus/.
  }
  deriving (Eq, Show)

instance Ord Acc where
  compare = Ord.comparing $ \acc -> (maxChainLength acc, maxForkDepth acc)

data Parted
  = MkParted !NetPartitionPlan !Acc !Bool !(NetClasses Acc)
    -- ^ 'Acc' at the start of the partition, Flag if an entire class started
    -- from genesis, an 'Acc' per class

-- | Internal accumulator of 'determineForkLength'
data St
  = Parted !Parted
  | Whole !Acc

-- | There was not yet an opportunity for healing after the network partition
--
-- Note that unlike 'maxForkDepth', this computed depth may be between chains
-- of a different length even if within @k@.
maxForkDepthBeforeHeal :: Parted -> Word64
maxForkDepthBeforeHeal (MkParted _npp acc0 genesisFlag accs)
    -- Some class started with all nodes at genesis and haven't yet had an
    -- opportunity to synchronize with the other nodes in the network. So the
    -- common intersection point /immediately/ after the partition (ie before
    -- healing) will be genesis.
  | genesisFlag   = clen ult
  | otherwise     =
    -- corner case: our upper bound on the fork depth must be at least as deep
    -- as the deepest fork among classes with the longest chain, since one of
    -- those will \"win\"
    max (maxForkDepth ult) $
    -- corner case: forks cannot be deeper than the longest chain
    min (clen ult) $
    -- The nodes haven't yet had a chance to heal, so the latest possible
    -- common intersection point will be the common point when the partition
    -- started.
    (clen ult - clen acc0)
    -- Futhermore, there may have already been a fork when the partition
    -- started, and it may have been preserved by the partition (ie two
    -- competitors were assigned to different classes).
    + maxForkDepth acc0
  where
    clen = maxChainLength
    ult = foldNetClasses max accs

-- | Model the healing communication after the network partition
heal :: SecurityParam -> Parted -> Acc
heal k parted@(MkParted _npp _acc0 _genesisFlag accs) =
  Acc
    { maxChainLength = clen ult
    , maxForkDepth   = refine $ maxForkDepthBeforeHeal parted
    }
  where
    clen   = maxChainLength
    fdepth = maxForkDepth

    (penult, ult) = max2NetClasses accs

    longestChainsAllFromUlt = clen ult > clen penult

    refine x
      | x > maxRollbacks k      = x
      | longestChainsAllFromUlt = fdepth ult
      | otherwise               = x

-- | Compute a bound for the length of the final forks off of the network's
-- nodes' common prefix
--
-- At the end of the test, the nodes' final chains will share a common prefix
-- (\"intersection\"), though it may be empty. Each node's current chain is a
-- fork off that prefix. No such fork will be longer than the result of this
-- function.
--
-- ASSUMPTION: The network connectivity is such that any block forged in a slot
-- will be fetched and possibly selected by every other node before the end of
-- the slot.
--
-- *How 'LeaderSchedule' affects this function*
--
-- A round-robin 'LeaderSchedule' will always reach consensus, so the fork
-- length will be @0@. For other 'LeaderSchedule's -- whether known /a priori/
-- (see "Test.Dynamic.LeaderSchedule") or /a posteriori/ (see
-- "Test.Dynamic.Praos") -- we often will not expect consensus. The key
-- difference is that a round-robin schedule always has exactly one leader per
-- slot. Arbitrary schedules instead may have 0 or multiple leaders.
--
-- A sequence of slots in which no slot has exactly one leader can drive a
-- network away from consensus. This function bounds how far from consensus the
-- network could be after the last simulated slot. It determines a conservative
-- upper bound on the length of the contended suffix of the nodes' final
-- chains. The bound may be less than, equal, or greater than @k@; regardless
-- of which, it should be used to test the nodes' final chains.
--
-- If such a sequence is long enough, it might even prevent the network from
-- every reaching consensus again: the nodes at the end of the sequence may
-- have chains that disagree by more than @k@ blocks, thereby exceeding the
-- security parameter @k@. In particular, if the bound determined by a
-- 'LeaderSchedule' is greater than @k@, then no extension of that
-- 'LeaderSchedule' can determine a lesser bound.
--
-- Multiple leaders create divergent chains as follows. Assume that every
-- leader of @s + 1@ begins the slot with a chain of length @n@, and that no
-- chain in the network has a greater length. Each leader forges a unique
-- block. A race condition between ChainSync/BlockFetch and forging makes it
-- possible, though relatively unlikely, that a leader would have received
-- another leader's new block before it forges its own. In that case, the new
-- leader will forged a block but then not select it, since it already selected
-- the other leader's new block. This function assumes the \"worst-case\" in
-- which both leaders make separate chains, thereby breaking consensus. Hence
-- it computes an upper bound. Each leader thus selects a new @n+1@-length
-- chain, and each non-leader will adopt one of them because they previously
-- had a chain of length @<= n@. Thus the network is likely not in consensus at
-- the end of a multi-leader slot.
--
-- The network will reestablish consensus at the end of a single-leader slot,
-- because the leader creates the only chain of length @n+1@ and all other
-- nodes thus select it.
--
-- In a slot with no leaders, all nodes will simply retain their current
-- chains.
--
-- *How 'NodeJoinPlan' affects this function*
--
-- Because the race condition between forging and ChainSync/BlockFetch is
-- consistently won by forging, a node that leads in the same slot it joins
-- always attempts to make a chain of length 1 (ASSUMPTION: new nodes start
-- with an empty chain). Except for the edge cases when the longest chains in
-- the network are of length 0 or 1, this means that leaders who are joining
-- can be disregarded.
--
determineForkLength ::
     SecurityParam
  -> NodeJoinPlan
  -> Maybe RefinedNetPartitionPlan
  -> LeaderSchedule
  -> NumBlocks
determineForkLength k njp mbRnpp (LeaderSchedule sched) =
    prj $ foldl' stepSt initialSt (Map.toAscList sched)
  where
    prj = NumBlocks . \case
        Parted parted -> maxForkDepthBeforeHeal parted
        Whole  acc    -> maxForkDepth acc

    mbNpp :: Maybe NetPartitionPlan
    mbNpp = fmap getRefinedNetPartitionPlan mbRnpp

    -- slot in which the node joins the network
    joinSlot :: CoreNodeId -> SlotNo
    joinSlot = coreNodeIdJoinSlot njp

    initialSt = Whole initial

    -- assume all nodes start at Genesis
    initial = Acc
      { maxChainLength = 0
      , maxForkDepth  = 0
      }

    -- handle start/stop of network partitions and also advancing the node
    -- classes
    stepSt st (slot, leaders) = stepSt' (preStep slot st) (slot, leaders)

    -- change Whole to Parted if the network partition is just now starting or
    -- vice versa if it just now ended
    --
    -- Note that the transition from Parted back to Whole corresponds to the
    -- exchange of healing messages after the network partition.
    preStep :: SlotNo -> St -> St
    preStep slot st = case st of
        Parted parted@(MkParted npp _acc0 _genesisFlag _accs)
            -- ending a network partition
          | slot == (succ . snd) (nppInterval npp) ->
            Whole $ heal k parted
          | otherwise -> st
        Whole acc
          | Just npp <- mbNpp
            -- starting a network partition
          , slot == fst (nppInterval npp) ->
            let earliests :: NetClasses SlotNo
                earliests = joinSlotsNetPartitionPlan njp npp

                -- these classes are starting from genesis, since they joined
                -- the network during the partition
                flags = fmap (slot <=) earliests
            in
            Parted $ MkParted npp acc (or flags) $
            flip fmap flags $ \flag -> if flag then initial else acc
              -- TODO truncate maxForkDepth for singleton classes?
          | otherwise -> st

    -- advance all classes independently
    stepSt' st (slot, leaders) = case st of
        Whole acc -> Whole $ step acc (slot, leaders)
        Parted (MkParted npp acc0 genesisFlag accs) ->
            Parted $ MkParted npp acc0 genesisFlag $
            (\acc ls -> step acc (slot, ls))
              `fmap` accs
              `apNetClasses` leaders2
          where
            leaders2 = foldl' (flip cons) nil leaders
              where
                nil = tabulateNetClasses $ \_ -> []
                cons l =
                    onNetPartition
                      (assignmentNetPartitionPlan npp l) (l:)

    -- this logic focuses on the new chains made in this slot that are longer
    -- than the longest chains from the previous slot
    step Acc{maxChainLength, maxForkDepth} (slot, leaders) =
        Acc
          { maxChainLength = grow   maxChainLength
          , maxForkDepth  = update maxForkDepth
          }
      where
        grow = if 0 == pullingAhead then id else (+ 1)

        update
            -- too late to reach consensus, so further diverge
          | maxForkDepth > maxRollbacks k = grow

            -- assume (common) worst-case: each leader creates a unique longer
            -- chain
          | pullingAhead >  1              = (+ 1)

            -- the sole leader creates the sole longer chain, bringing the
            -- network into consensus
          | pullingAhead == 1              = const 0

            -- there will be multiple longest chains that disagree on at least
            -- the latest block
            --
            -- Note that pullingAhead == 0 by the preceding guards
          | pullingEven  >  0              = max 1

            -- no nodes are extending their chain, so the longest chains are
            -- the same as in the previous slot
          | otherwise                      = id

        -- how many leaders are forging a block onto a @maxForkDepth@-chain
        pullingAhead = nlOld + nlNew (maxChainLength == 0)
        -- how many leaders are forging a block onto a @maxForkDepth - 1@-chain
        pullingEven  = nlNew (maxChainLength == 1)

        -- how many leaders joined before this slot
        nlOld = length $ filter ((< slot) . joinSlot) leaders
        nlNew b
          | not b     = 0
            -- how many leaders are joining during this slot; these might not
            -- be relevant
            --
            -- A node leading the same slot it joins always forges a 1-block
            -- chain; there's actually a race-condition between blocking
            -- forging and BlockFetch/ChainSync, but forging always wins in the
            -- current test framework implementation.
          | otherwise = length $ filter ((== slot) . joinSlot) leaders
