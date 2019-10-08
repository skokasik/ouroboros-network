{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Ouroboros.Consensus.Ledger.Mock.Stake (
    -- * Stakeholders
    StakeHolder(..)
    -- * Address distribution
  , AddrDist
    -- * Stake distribution
  , StakeDist(..)
  , stakeWithDefault
  , relativeStakes
  , totalStakes
  , equalStakeDist
  , genesisStakeDist
  ) where

import           Codec.Serialise (Serialise)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Ledger.Mock.Address
import           Ouroboros.Consensus.Ledger.Mock.UTxO
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))

{-------------------------------------------------------------------------------
  Stakeholders
-------------------------------------------------------------------------------}

data StakeHolder =
    -- | Stake of a core node
    StakeCore CoreNodeId

    -- | Stake for everybody else (we don't need to distinguish)
  | StakeEverybodyElse
  deriving (Show, Eq, Ord)

{-------------------------------------------------------------------------------
  Stake distribution
-------------------------------------------------------------------------------}

newtype StakeDist = StakeDist { stakeDistToIntMap :: IntMap Rational }
  deriving (Show, Eq, Serialise, NoUnexpectedThunks)

stakeWithDefault :: Rational -> CoreNodeId -> StakeDist -> Rational
stakeWithDefault d cid =
    IntMap.findWithDefault d (fromIntegral (unCoreNodeId cid)) .
    stakeDistToIntMap

relativeStakes :: Map StakeHolder Int -> StakeDist
relativeStakes m = StakeDist $
   let totalStake    = fromIntegral $ sum $ Map.elems m
   in  IntMap.fromList
       [ (fromIntegral (unCoreNodeId cid), fromIntegral stake / totalStake)
       | (StakeCore cid, stake) <- Map.toList m
       ]

-- | Compute stakes of all nodes
--
-- The 'Nothing' value holds the total stake of all addresses that don't
-- get mapped to a NodeId.
totalStakes :: Map Addr NodeId -> Utxo -> Map StakeHolder Int
totalStakes addrDist = foldl f Map.empty
 where
   f :: Map StakeHolder Int -> TxOut -> Map StakeHolder Int
   f m (a, stake) = case Map.lookup a addrDist of
       Just (CoreId cid) -> Map.insertWith (+) (StakeCore cid)    stake m
       _                 -> Map.insertWith (+) StakeEverybodyElse stake m

-- | Stake distribution where every address has equal state
equalStakeDist :: AddrDist -> StakeDist
equalStakeDist = StakeDist
               . IntMap.fromList
               . mapMaybe (nodeStake . snd)
               . Map.toList
  where
    nodeStake :: NodeId -> Maybe (Int, Rational)
    nodeStake (RelayId _)  = Nothing
    nodeStake (CoreId cid) = Just (fromIntegral (unCoreNodeId cid), 1)

-- | Genesis stake distribution
genesisStakeDist :: AddrDist -> StakeDist
genesisStakeDist addrDist =
    relativeStakes (totalStakes addrDist (genesisUtxo addrDist))
