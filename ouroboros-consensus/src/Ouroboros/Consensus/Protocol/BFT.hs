{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE RecordWildCards         #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Protocol.BFT (
    Bft
  , BftFields(..)
  , BftParams(..)
  , BftIsALeaderOrNot
  , BftIsLeader(..)
  , forgeBftFields
    -- * Classes
  , BftCrypto(..)
  , BftStandardCrypto
  , BftMockCrypto
  , HeaderSupportsBft(..)
    -- * Type instances
  , NodeConfig(..)
  ) where

import           Cardano.Crypto.DSIGN.Class
import           Cardano.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Cardano.Crypto.DSIGN.Mock (MockDSIGN)
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.Monad.Except
import           Crypto.Random (MonadRandom)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy
import           Data.Typeable
import           GHC.Generics (Generic)

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
                     (NumCoreNodes (..))
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), RelayNodeId)
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  Fields BFT requires in a block
-------------------------------------------------------------------------------}

data BftFields c toSign = BftFields {
      bftSignature :: !(SignedDSIGN (BftDSIGN c) toSign)
    }
  deriving (Generic)

deriving instance BftCrypto c => Show (BftFields c toSign)
deriving instance BftCrypto c => Eq   (BftFields c toSign)

-- We use the generic implementation, but override 'showTypeOf' to show @c@
instance (BftCrypto c, Typeable toSign) => NoUnexpectedThunks (BftFields c toSign) where
  showTypeOf _ = show $ typeRep (Proxy @(BftFields c))

class ( HasHeader hdr
      , SignedHeader hdr
      , Signable (BftDSIGN c) (Signed hdr)
      ) => HeaderSupportsBft c hdr where
  headerBftFields :: NodeConfig (Bft c) -> hdr -> BftFields c (Signed hdr)

forgeBftFields :: ( MonadRandom m
                  , BftCrypto c
                  , Signable (BftDSIGN c) toSign
                  )
               => NodeConfig (Bft c)
               -> BftIsLeader c
               -> toSign
               -> m (BftFields c toSign)
forgeBftFields _ BftIsLeader { bftSignKey } toSign = do
      signature <- signedDSIGN toSign bftSignKey -- CONTINUE
      return $ BftFields {
          bftSignature = signature
        }

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

-- | Basic BFT
--
-- Basic BFT is very simple:
--
-- * No support for delegation (and hence has no need for a ledger view)
-- * Requires round-robin block signing throughout (and so has no
--   need for any chain state or cryptographic leader proofs).
-- * Does not use any stateful crypto (and so has no need for node state)
data Bft c

-- | Protocol parameters
data BftParams = BftParams {
      -- | Security parameter
      --
      -- Although the protocol proper does not have such a security parameter,
      -- we insist on it.
      bftSecurityParam :: !SecurityParam

      -- | Number of core nodes
    , bftNumCoreNodes  :: !NumCoreNodes
    }
  deriving (Generic, NoUnexpectedThunks)


-- | If we are a core node (i.e. a block producing node) we know which core
-- node we are, and we have the operational key pair.
--
data BftIsLeader c = BftIsLeader {
      bftCoreNodeId :: !CoreNodeId
    , bftSignKey    :: !(SignKeyDSIGN (BftDSIGN c))
    }
  deriving (Generic)

instance BftCrypto c => NoUnexpectedThunks (BftIsLeader c)
  -- use generic instance

type BftIsALeaderOrNot c = IsALeaderOrNot RelayNodeId (BftIsLeader c)

instance BftCrypto c => OuroborosTag (Bft c) where
  -- | (Static) node configuration
  data NodeConfig (Bft c) = BftNodeConfig {
        bftParams   :: !BftParams
      , bftIsLeader :: !(BftIsALeaderOrNot c)
      , bftVerKeys  :: !(Map CoreNodeId (VerKeyDSIGN (BftDSIGN c)))
      }
    deriving (Generic)

  type ValidationErr   (Bft c) = BftValidationErr
  type SupportedHeader (Bft c) = HeaderSupportsBft c
  type NodeState       (Bft c) = ()
  type LedgerView      (Bft c) = ()
  type IsLeader        (Bft c) = BftIsLeader c
  type ChainState      (Bft c) = ()

  protocolSecurityParam = bftSecurityParam . bftParams

  checkIsLeader BftNodeConfig{..} (SlotNo n) _l _cs = do
      return $ case bftIsLeader of
        IsALeader credentials@BftIsLeader { bftCoreNodeId = CoreNodeId i }
          | n `mod` unNumCoreNodes bftNumCoreNodes == i
          -> Just credentials
          | otherwise
          -> Nothing
        IsNotALeader _
          -> Nothing
    where
      BftParams{..}  = bftParams

  applyChainState cfg@BftNodeConfig{..} _l b _cs = do
      -- TODO: Should deal with unknown node IDs
      case verifySignedDSIGN
           (bftVerKeys Map.! expectedLeader)
           (headerSigned b)
           bftSignature of
        Right () -> return ()
        Left err -> throwError $ BftInvalidSignature err
    where
      BftParams{..}  = bftParams
      BftFields{..}  = headerBftFields cfg b
      SlotNo n       = blockSlot b
      expectedLeader = CoreNodeId (n `mod` unNumCoreNodes bftNumCoreNodes)

  rewindChainState _ _ _ = Just ()

instance BftCrypto c => NoUnexpectedThunks (NodeConfig (Bft c))
  -- use generic instance

{-------------------------------------------------------------------------------
  BFT specific types
-------------------------------------------------------------------------------}

data BftValidationErr = BftInvalidSignature String
  deriving (Show)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

-- | Crypto primitives required by BFT
class ( Typeable c
      , DSIGNAlgorithm (BftDSIGN c)
      , Condense (SigDSIGN (BftDSIGN c))
      , NoUnexpectedThunks (SigDSIGN (BftDSIGN c))
      ) => BftCrypto c where
  type family BftDSIGN c :: *

data BftStandardCrypto
data BftMockCrypto

instance BftCrypto BftStandardCrypto where
  type BftDSIGN BftStandardCrypto = Ed448DSIGN

instance BftCrypto BftMockCrypto where
  type BftDSIGN BftMockCrypto = MockDSIGN

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance BftCrypto c => Condense (BftFields c toSign) where
  condense BftFields{..} = condense bftSignature
