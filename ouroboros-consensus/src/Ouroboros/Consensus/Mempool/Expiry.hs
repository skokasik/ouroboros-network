{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Mempool transaction expiry.
module Ouroboros.Consensus.Mempool.Expiry
  ( ExpiryThreshold (NoExpiryThreshold, ExpiryThreshold)
  , ExpiryTime (..)
  , expiryTime
  ) where

import           Cardano.Prelude (Generic, NoUnexpectedThunks)

import           Data.Time.Clock (DiffTime)
import           GHC.Stack (HasCallStack)

{-# COMPLETE NoExpiryThreshold, ExpiryThreshold #-}

-- | The time that must pass before a transaction should be considered
-- expired.
data ExpiryThreshold
  = NoExpiryThreshold
  -- ^ Transactions in the mempool do not expire.
  -- This is useful for tests in which we don't care about transaction expiry.
  | UnsafeExpiryThreshold !DiffTime
  -- ^ Transactions in the mempool expire after a given amount of time passes.
  -- n.b. This constructor is considered unsafe because it can accept values
  -- that are less than or equal to 0. Because of this, it is preferred to use
  -- the smart constructor, 'ExpiryThreshold'.
  deriving Show

-- | A smart constructor for 'ExpiryThreshold' which ensures that the provided
-- 'DiffTime' value is greater than 0.
pattern ExpiryThreshold :: HasCallStack => DiffTime -> ExpiryThreshold
pattern ExpiryThreshold n <- UnsafeExpiryThreshold n where
  ExpiryThreshold amountOfTime
    | amountOfTime > 0 = UnsafeExpiryThreshold amountOfTime
    | otherwise = error "ExpiryThreshold: Time value must be greater than 0"

-- | A point in time that indicates at which point all prior transactions are
-- to be expired.
data ExpiryTime
  = NoExpiryTime
  -- ^ No transactions should be expired. This can happen when we haven't
  -- progressed far enough along the chain to meet the 'ExpiryThreshold'.
  | ExpiryTime !DiffTime
  -- ^ The time from which all prior transactions are expired.
  deriving (Eq, Ord, Show, Generic, NoUnexpectedThunks)

-- TODO @intricate: Smart constructor for ExpiryTime

-- | Calculate the expiry time.
-- Time values less than or equal to this are considered to be expired.
expiryTime :: DiffTime
           -- ^ The current time
           -> ExpiryThreshold
           -- ^ The expiry threshold
           -> ExpiryTime
expiryTime currentTime expThreshold = case expThreshold of
  NoExpiryThreshold -> NoExpiryTime
  ExpiryThreshold et -> ExpiryTime (currentTime + et)
