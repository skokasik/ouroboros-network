{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ouroboros.Consensus.NodeId (
    -- * Node IDs
    NodeId (..)
  , CoreNodeId (..)
  , RelayNodeId (..)
  ) where

import           Codec.Serialise (Serialise)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Util.Condense (Condense (..))

{-------------------------------------------------------------------------------
  Node IDs
-------------------------------------------------------------------------------}

data NodeId
  = CoreId  CoreNodeId
  | RelayId RelayNodeId
  deriving (Eq, Ord, Show, Generic, NoUnexpectedThunks)

instance Condense NodeId where
  condense (CoreId  i) = "c" ++ condense i
  condense (RelayId i) = "r" ++ condense i

-- | Core node ID
newtype CoreNodeId = CoreNodeId { unCoreNodeId :: Word64 }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Condense, Serialise, NoUnexpectedThunks)

-- | Relay node ID
newtype RelayNodeId = RelayNodeId { unRelayNodeId :: Word64 }
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Condense, Serialise, NoUnexpectedThunks)
