{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Test.Dynamic.Util.OutagesPlan (
  OutageEdge,
  OutageInterval (..),
  OutagesPlan (..),
  OutagesPlanEI,
  OutagesPlanIE,
  emptyOutagesPlan,
  insertOutage,
  nextSlotView,
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.NodeId

-- | The slot interval of a planned outage
--
-- NOTE The @'OutageInterval' firstSlot lastSlot@ argument order is intuitive
-- and derives the necessary 'Ord' instance. It is only coincidentally the
-- alphabetical ordering.
--
data OutageInterval = OutageInterval
  { outageFirst :: !SlotNo
  , outageLast  :: !SlotNo
  }
  deriving (Eq, Ord)

-- | Suppressing field names
--
instance Show OutageInterval where
  showsPrec p (OutageInterval s e) =
      showParen (p < 11) $
          showString "OutageInterval " . f s . showChar ' ' . f e
    where
      f (SlotNo x) = showParen True $ showString "SlotNo " . showsPrec 0 x

touch :: OutageInterval -> OutageInterval -> Bool
touch (OutageInterval s1 e1) (OutageInterval s2 e2) =
    s2 <= e1 + 1 && s1 <= e2 + 1

-- | An ordered pair of node IDs identifies a directed edge in the node
-- network's topology
--
-- The first node runs the mini protocol servers, and the second the clients.
--
type OutageEdge = (CoreNodeId, CoreNodeId)

-- | The planned outages
--
-- A relation between edges and the intervals of planned outages.
--
-- INVARIANT: No two intervals related to the same edge overlap or even abut.
--
-- INVARIANT: The relation is stored as its two inverse functions; the sets are
-- non-empty.
--
data OutagesPlan = OutagesPlan_Unsafe OutagesPlanIE OutagesPlanEI
  deriving (Eq, Show)

-- | See 'OutagesPlan'.
--
type OutagesPlanIE = Map OutageInterval (Set OutageEdge)

-- | See 'OutagesPlan'.
--
type OutagesPlanEI = Map OutageEdge (Set OutageInterval)

emptyOutagesPlan :: OutagesPlan
emptyOutagesPlan = OutagesPlan_Unsafe Map.empty Map.empty

-- | Add an outage to the plan
--
-- Maintains the invariants of 'OutagesPlan'. For example, the invariants might
-- require that the new outage be merged with existing outages.
--
insertOutage :: OutageEdge -> OutageInterval -> OutagesPlan -> OutagesPlan
insertOutage edge new (OutagesPlan_Unsafe mIE mEI) =
    OutagesPlan_Unsafe mIE' mEI'
  where
    dels :: Set OutageInterval
    ins  :: OutageInterval
    mEI' :: OutagesPlanEI
    ((dels, ins), mEI') =
        Map.alterF (fmap Just . updEI . fromMaybe Set.empty) edge mEI

    mIE' :: OutagesPlanIE
    mIE' = mapSetInsert ins edge $ foldl updIE mIE dels

    updEI ::
         Set OutageInterval
      -> ((Set OutageInterval, OutageInterval), Set OutageInterval)
    updEI os = ((merges, merged), Set.insert merged keeps)
      where
        (merges, keeps) = Set.partition (touch new) os
        merged
          | Set.null merges = new
          | otherwise       =
            OutageInterval
                (minimum $ outageFirst <$> Set.toList merges)
                (maximum $ outageLast  <$> Set.toList merges)

    updIE :: OutagesPlanIE -> OutageInterval -> OutagesPlanIE
    updIE acc del = mapSetDelete del edge acc

-- | Pop an outage planned to start as soon as any other
--
nextView :: OutagesPlan -> Maybe ((OutageInterval, OutageEdge), OutagesPlan)
nextView (OutagesPlan_Unsafe mIE mEI) =
    case Map.minViewWithKey mIE of
        Nothing              -> Nothing
        Just ((o, es), mIE') -> case Set.minView es of
            Nothing       -> Nothing   -- an ignorable invariant violation
            Just (e, es') -> Just
                ( (o, e)
                , OutagesPlan_Unsafe
                      (Map.insert o es' mIE')
                      (mapSetDelete e o mEI)
                )

-- | Pop all outages planned to start next
--
-- Returns @(s1, s2s, plan')@: the first slot of all the outages, the last slot
-- of the outage for each edge, and the remaining plan.
--
-- INVARIANT @maybe True (> s1) ('nextSlotView' plan')@
--
-- INVARIANT @not (Map.null s2s)@
--
-- INVARIANT @foldr (\(e, s2) -> 'insertOutage' e ('OutageInterval' s1 s2)) plan' (Map.toList s2s) = plan@
--
nextSlotView ::
     OutagesPlan
  -> Maybe (SlotNo, Map OutageEdge SlotNo, OutagesPlan)
nextSlotView = fmap go0 . nextView
  where
    go0 ::
         ((OutageInterval, OutageEdge), OutagesPlan)
      -> (SlotNo, Map OutageEdge SlotNo, OutagesPlan)
    go0 ((OutageInterval s1 s2, e), plan) = go s1 (Map.singleton e s2) plan

    go ::
         SlotNo
      -> Map OutageEdge SlotNo
      -> OutagesPlan
      -> (SlotNo, Map OutageEdge SlotNo, OutagesPlan)
    go s1 !s2s plan = case nextView plan of
        Just ((o, e), plan')
          | outageFirst o == s1 -> go s1 (Map.insert e (outageLast o) s2s) plan'
        _ -> (s1, s2s, plan)

{-------------------------------------------------------------------------------
  Set-valued maps
-------------------------------------------------------------------------------}

-- | Deleting the element in a singleton set drops the mapping
--
mapSetDelete :: (Ord k, Ord v) => k -> v -> Map k (Set v) -> Map k (Set v)
mapSetDelete k v m = Map.alter f k m
  where
    f = \case
        Nothing -> Nothing
        Just vs -> nonEmptySet $ Set.delete v vs

    nonEmptySet vs = if Set.null vs then Nothing else Just vs

mapSetInsert :: (Ord k, Ord v) => k -> v -> Map k (Set v) -> Map k (Set v)
mapSetInsert k v m = Map.alter (Just . f) k m
  where
    f = \case
        Nothing -> Set.singleton v
        Just vs -> Set.insert v vs
