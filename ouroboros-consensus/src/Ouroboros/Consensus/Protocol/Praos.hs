{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE UndecidableSuperClasses   #-}

-- | Proof of concept implementation of Praos
module Ouroboros.Consensus.Protocol.Praos (
    Praos
  , PraosFields(..)
  , PraosExtraFields(..)
  , PraosParams(..)
  , PraosNodeState(..)
  , forgePraosFields
    -- * Tags
  , PraosCrypto(..)
  , PraosStandardCrypto
  , PraosMockCrypto
  , PraosValidateView(..)
  , praosValidateView
    -- * Type instances
  , NodeConfig(..)
  , BlockInfo(..)
  ) where

import           Cardano.Binary (ToCBOR (..))
import           Codec.Serialise (Serialise (..))
import           Control.Monad (unless)
import           Control.Monad.Except (throwError)
import           Control.Monad.Identity (runIdentity)
import           Crypto.Random (MonadRandom)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Proxy (Proxy (..))
import           Data.Typeable
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Numeric.Natural

import           Cardano.Crypto.DSIGN.Ed448 (Ed448DSIGN)
import           Cardano.Crypto.Hash.Class (HashAlgorithm (..), fromHash, hash)
import           Cardano.Crypto.Hash.MD5 (MD5)
import           Cardano.Crypto.Hash.SHA256 (SHA256)
import           Cardano.Crypto.KES.Class
import           Cardano.Crypto.KES.Mock
import           Cardano.Crypto.KES.Simple
import           Cardano.Crypto.VRF.Class
import           Cardano.Crypto.VRF.Mock (MockVRF)
import           Cardano.Crypto.VRF.Simple (SimpleVRF)
import           Cardano.Prelude (NoUnexpectedThunks (..), fromMaybe)

import           Ouroboros.Network.Block (HasHeader (..), SlotNo (..),
                     pointSlot)
import           Ouroboros.Network.Point (WithOrigin (At))

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Ledger.Mock.Stake
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.Signed
import           Ouroboros.Consensus.Util.Condense

import           Ouroboros.Storage.Common (EpochNo (..), EpochSize (..))
import           Ouroboros.Storage.EpochInfo (EpochInfo (..),
                     fixedSizeEpochInfo)

{-------------------------------------------------------------------------------
  Fields required by Praos in the header
-------------------------------------------------------------------------------}

-- | The fields that Praos required in the header
data PraosFields c toSign = PraosFields {
      praosSignature   :: SignedKES (PraosKES c) toSign
    , praosExtraFields :: PraosExtraFields c
    }
  deriving (Generic)

instance (PraosCrypto c, Typeable toSign) => NoUnexpectedThunks (PraosFields c toSign)
  -- use generic instance

-- | Fields that should be included in the signature
data PraosExtraFields c = PraosExtraFields {
      praosCreator :: CoreNodeId
    , praosRho     :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosY       :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    }
  deriving (Generic)

instance PraosCrypto c => NoUnexpectedThunks (PraosExtraFields c)
  -- use generic instance

data PraosValidateView c =
    forall signed. Cardano.Crypto.KES.Class.Signable (PraosKES c) signed
                => PraosValidateView SlotNo (PraosFields c signed) signed

-- | Convenience constructor for 'PraosValidateView'
praosValidateView :: ( HasHeader    hdr
                     , SignedHeader hdr
                     , Cardano.Crypto.KES.Class.Signable (PraosKES c) (Signed hdr)
                     )
                  => (hdr -> PraosFields c (Signed hdr))
                  -> (hdr -> PraosValidateView c)
praosValidateView getFields hdr =
    PraosValidateView (blockSlot hdr) (getFields hdr) (headerSigned hdr)

forgePraosFields :: ( HasNodeState (Praos c) m
                    , MonadRandom m
                    , PraosCrypto c
                    , Cardano.Crypto.KES.Class.Signable (PraosKES c) toSign
                    )
                 => NodeConfig (Praos c)
                 -> PraosProof c
                 -> (PraosExtraFields c -> toSign)
                 -> m (PraosFields c toSign)
forgePraosFields PraosNodeConfig{..} PraosProof{..} mkToSign = do
    keyKES <- unPraosNodeState <$> getNodeState
    let signedFields = PraosExtraFields {
          praosCreator = praosLeader
        , praosRho     = praosProofRho
        , praosY       = praosProofY
        }
    m <- signedKES
           ()
           (fromIntegral (unSlotNo praosProofSlot))
           (mkToSign signedFields)
           keyKES
    case m of
      Nothing -> error "mkOutoborosPayload: signedKES failed"
      Just signature -> do
        -- TODO : We should not update the key on each signing, but X slots
        -- (for configurable param X)
        newKey <- fromMaybe (error "mkOutoborosPayload: updateKES failed") <$> updateKES () keyKES
        putNodeState (PraosNodeState newKey)
        return $ PraosFields {
            praosSignature    = signature
          , praosExtraFields = signedFields
          }

{-------------------------------------------------------------------------------
  Praos specific types
-------------------------------------------------------------------------------}

data VRFType = NONCE | TEST
    deriving (Show, Eq, Ord, Generic, NoUnexpectedThunks)

instance Serialise VRFType
  -- use generic instance

instance ToCBOR VRFType where
  -- This is a cheat, and at some point we probably want to decide on Serialise/ToCBOR
  toCBOR = encode

deriving instance PraosCrypto c => Show (PraosExtraFields c)
deriving instance PraosCrypto c => Eq   (PraosExtraFields c)

deriving instance PraosCrypto c => Show (PraosFields c toSign)
deriving instance PraosCrypto c => Eq   (PraosFields c toSign)

data PraosProof c = PraosProof {
      praosProofRho  :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosProofY    :: CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType)
    , praosLeader    :: CoreNodeId
    , praosProofSlot :: SlotNo
    }

data PraosValidationError c =
      PraosInvalidSlot SlotNo SlotNo
    | PraosUnknownCoreId CoreNodeId
    | PraosInvalidSig String (VerKeyKES (PraosKES c)) Natural (SigKES (PraosKES c))
    | PraosInvalidCert (VerKeyVRF (PraosVRF c)) (Natural, SlotNo, VRFType) Natural (CertVRF (PraosVRF c))
    | PraosInsufficientStake Double Natural
    deriving (Generic)

-- We override 'showTypeOf' to make sure to show @c@
instance PraosCrypto c => NoUnexpectedThunks (PraosValidationError c) where
  showTypeOf _ = show $ typeRep (Proxy @(PraosValidationError c))

deriving instance PraosCrypto c => Show (PraosValidationError c)
deriving instance PraosCrypto c => Eq   (PraosValidationError c)

data BlockInfo c = BlockInfo
    { biSlot  :: !SlotNo
    , biRho   :: !(CertifiedVRF (PraosVRF c) (Natural, SlotNo, VRFType))
    , biStake :: !StakeDist
    }
  deriving (Generic)

deriving instance PraosCrypto c => Show (BlockInfo c)
deriving instance PraosCrypto c => Eq   (BlockInfo c)
deriving instance PraosCrypto c => NoUnexpectedThunks (BlockInfo c)

{-------------------------------------------------------------------------------
  Protocol proper
-------------------------------------------------------------------------------}

data Praos c

-- | Praos parameters that are node independent
data PraosParams = PraosParams {
      praosLeaderF       :: !Double
    , praosSecurityParam :: !SecurityParam
    , praosSlotsPerEpoch :: !Word64
    , praosLifetimeKES   :: !Natural
    , praosSlotLength    :: !SlotLength
    }
  deriving (Generic, NoUnexpectedThunks)

newtype PraosNodeState c = PraosNodeState {
      unPraosNodeState :: SignKeyKES (PraosKES c)
    }
  deriving (Generic)

-- We override 'showTypeOf' to make sure to show @c@
instance PraosCrypto c => NoUnexpectedThunks (PraosNodeState c) where
  showTypeOf _ = show $ typeRep (Proxy @(PraosNodeState c))

data instance NodeConfig (Praos c) = PraosNodeConfig
  { praosParams       :: !PraosParams
  , praosInitialEta   :: !Natural
  , praosInitialStake :: !StakeDist
  , praosNodeId       :: !NodeId
  , praosSignKeyVRF   :: !(SignKeyVRF (PraosVRF c))
  , praosVerKeys      :: !(Map CoreNodeId (VerKeyKES (PraosKES c), VerKeyVRF (PraosVRF c)))
  }
  deriving (Generic)

instance PraosCrypto c => OuroborosTag (Praos c) where
  protocolSecurityParam =                        praosSecurityParam . praosParams
  protocolSlotLengths   = singletonSlotLengths . praosSlotLength    . praosParams

  type NodeState     (Praos c) = PraosNodeState c
  type LedgerView    (Praos c) = StakeDist
  type IsLeader      (Praos c) = PraosProof c
  type ValidationErr (Praos c) = PraosValidationError c
  type ValidateView  (Praos c) = PraosValidateView    c
  type ChainState    (Praos c) = [BlockInfo c]

  checkIsLeader cfg@PraosNodeConfig{..} slot _u cs =
    case praosNodeId of
        RelayId _  -> return Nothing
        CoreId nid -> do
          let (rho', y', t) = rhoYT cfg cs slot nid
          rho <- evalCertified () rho' praosSignKeyVRF
          y   <- evalCertified () y'   praosSignKeyVRF
          return $ if fromIntegral (certifiedNatural y) < t
              then Just PraosProof {
                       praosProofRho  = rho
                     , praosProofY    = y
                     , praosLeader    = nid
                     , praosProofSlot = slot
                     }
              else Nothing

  applyChainState cfg@PraosNodeConfig{..}
                  sd
                  (PraosValidateView slot PraosFields{..} toSign)
                  cs = do
    let PraosExtraFields{..} = praosExtraFields
        nid                  = praosCreator

    -- check that the new block advances time
    case cs of
        (c : _)
            | biSlot c >= slot -> throwError $ PraosInvalidSlot slot (biSlot c)
        _                      -> return ()

    -- check that block creator is a known core node
    (vkKES, vkVRF) <- case Map.lookup nid praosVerKeys of
        Nothing  -> throwError $ PraosUnknownCoreId nid
        Just vks -> return vks

    -- verify block signature
    case verifySignedKES
           ()
           vkKES
           (fromIntegral $ unSlotNo slot)
           toSign
           praosSignature of
       Right () -> return ()
       Left err -> throwError $ PraosInvalidSig
                                  err
                                  vkKES
                                  (fromIntegral $ unSlotNo slot)
                                  (getSig praosSignature)

    let (rho', y', t) = rhoYT cfg cs slot nid

    -- verify rho proof
    unless (verifyCertified () vkVRF rho' praosRho) $
        throwError $ PraosInvalidCert
            vkVRF
            rho'
            (certifiedNatural praosRho)
            (certifiedProof praosRho)

    -- verify y proof
    unless (verifyCertified () vkVRF y' praosY) $
        throwError $ PraosInvalidCert
            vkVRF
            y'
            (certifiedNatural praosY)
            (certifiedProof praosY)

    -- verify stake
    unless (fromIntegral (certifiedNatural praosY) < t) $
        throwError $ PraosInsufficientStake t $ certifiedNatural praosY

    let !bi = BlockInfo
            { biSlot  = slot
            , biRho   = praosRho
            , biStake = sd
            }

    return $ bi : cs

  -- Rewind the chain state
  --
  -- At the moment, this implementation of Praos keeps the full history of the
  -- chain state since the dawn of time (#248). For this reason rewinding is
  -- very simple, and we can't get to a point where we can't roll back more
  -- (unless the slot number never occurred, but that would be a bug in the
  -- caller). Once we limit the history we keep, this function will become
  -- more complicated.
  --
  -- We don't roll back to the exact slot since that slot might not have been
  -- filled; instead we roll back the the block just before it.
  rewindChainState PraosNodeConfig{..} cs rewindTo =
      -- This may drop us back to the empty list if we go back to genesis
      Just $ dropWhile (\bi -> At (biSlot bi) > pointSlot rewindTo) cs

  -- (Standard) Praos uses the standard chain selection rule, so no need to
  -- override (though see note regarding clock skew).

instance PraosCrypto c => NoUnexpectedThunks (NodeConfig (Praos c))
  -- use generic instance

slotEpoch :: NodeConfig (Praos c) -> SlotNo -> EpochNo
slotEpoch PraosNodeConfig{..} s =
    runIdentity $ epochInfoEpoch epochInfo s
  where
    epochInfo = fixedSizeEpochInfo (EpochSize praosSlotsPerEpoch)
    PraosParams{..} = praosParams

blockInfoEpoch :: NodeConfig (Praos c) -> BlockInfo c -> EpochNo
blockInfoEpoch l = slotEpoch l . biSlot

epochFirst :: NodeConfig (Praos c) -> EpochNo -> SlotNo
epochFirst PraosNodeConfig{..} e =
    runIdentity $ epochInfoFirst epochInfo e
  where
    epochInfo = fixedSizeEpochInfo (EpochSize praosSlotsPerEpoch)
    PraosParams{..} = praosParams

infosSlice :: SlotNo -> SlotNo -> [BlockInfo c] -> [BlockInfo c]
infosSlice from to xs = takeWhile (\b -> biSlot b >= from)
                      $ dropWhile (\b -> biSlot b > to) xs

infosEta :: forall c. (PraosCrypto c)
         => NodeConfig (Praos c)
         -> [BlockInfo c]
         -> EpochNo
         -> Natural
infosEta l _  0 = praosInitialEta l
infosEta l xs e =
    let e'   = e - 1
        eta' = infosEta l xs e'
        from = epochFirst l e'
        n    = div (2 * praosSlotsPerEpoch) 3
        to   = SlotNo $ unSlotNo from + fromIntegral n
        rhos = reverse [biRho b | b <- infosSlice from to xs]
    in  fromHash $ hash @(PraosHash c) (eta', e, rhos)
  where
    PraosParams{..} = praosParams l

infosStake :: NodeConfig (Praos c) -> [BlockInfo c] -> EpochNo -> StakeDist
infosStake s@PraosNodeConfig{..} xs e = case ys of
    []                  -> praosInitialStake
    (BlockInfo{..} : _) -> biStake
  where
    PraosParams{..} = praosParams

    e' = if e >= 2 then EpochNo (unEpochNo e - 2) else 0
    ys = dropWhile (\b -> blockInfoEpoch s b > e') xs

phi :: NodeConfig (Praos c) -> Rational -> Double
phi PraosNodeConfig{..} r = 1 - (1 - praosLeaderF) ** fromRational r
  where
    PraosParams{..} = praosParams

leaderThreshold :: forall c. PraosCrypto c
                => NodeConfig (Praos c)
                -> [BlockInfo c]
                -> SlotNo
                -> CoreNodeId
                -> Double
leaderThreshold st xs s n =
    let a = stakeWithDefault 0 n $ infosStake st xs (slotEpoch st s)
    in  2 ^ (byteCount (Proxy :: Proxy (PraosHash c)) * 8) * phi st a

rhoYT :: PraosCrypto c
      => NodeConfig (Praos c)
      -> [BlockInfo c]
      -> SlotNo
      -> CoreNodeId
      -> ( (Natural, SlotNo, VRFType)
         , (Natural, SlotNo, VRFType)
         , Double
         )
rhoYT st xs s nid =
    let e   = slotEpoch st s
        eta = infosEta st xs e
        rho = (eta, s, NONCE)
        y   = (eta, s, TEST)
        t   = leaderThreshold st xs s nid
    in  (rho, y, t)

{-------------------------------------------------------------------------------
  Crypto models
-------------------------------------------------------------------------------}

class ( KESAlgorithm  (PraosKES  c)
      , VRFAlgorithm  (PraosVRF  c)
      , HashAlgorithm (PraosHash c)
      , Typeable c
      , Typeable (PraosVRF c)
      , Condense (SigKES (PraosKES c))
      , Cardano.Crypto.VRF.Class.Signable (PraosVRF c) (Natural, SlotNo, VRFType)
      , ContextKES (PraosKES c) ~ ()
      , ContextVRF (PraosVRF c) ~ ()
      ) => PraosCrypto (c :: *) where
  type family PraosKES  c :: *
  type family PraosVRF  c :: *
  type family PraosHash c :: *

data PraosStandardCrypto
data PraosMockCrypto

instance PraosCrypto PraosStandardCrypto where
  type PraosKES  PraosStandardCrypto = SimpleKES Ed448DSIGN
  type PraosVRF  PraosStandardCrypto = SimpleVRF
  type PraosHash PraosStandardCrypto = SHA256

instance PraosCrypto PraosMockCrypto where
  type PraosKES  PraosMockCrypto = MockKES
  type PraosVRF  PraosMockCrypto = MockVRF
  type PraosHash PraosMockCrypto = MD5

{-------------------------------------------------------------------------------
  Condense
-------------------------------------------------------------------------------}

instance PraosCrypto c => Condense (PraosFields c toSign) where
   condense PraosFields{..} = condense praosSignature
