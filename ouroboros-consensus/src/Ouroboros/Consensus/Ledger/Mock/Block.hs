{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyCase                  #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Simple block to go with the mock ledger
--
-- None of the definitions in this module depend on, or even refer to, any
-- specific consensus protocols.
module Ouroboros.Consensus.Ledger.Mock.Block (
    SimpleBlock
  , SimpleHeader
  , SimpleBlock'(..)
  , Header(..)
  , SimpleStdHeader(..)
  , SimpleBody(..)
  , SimpleHash
    -- * Working with 'SimpleBlock'
  , mkSimpleHeader
  , matchesSimpleHeader
  , countSimpleGenTxs
    -- * 'UpdateLedger'
  , LedgerState(..)
  , LedgerConfig(..)
  , updateSimpleLedgerState
  , genesisSimpleLedgerState
    -- * 'ApplyTx' (mempool support)
  , GenTx(..)
  , mkSimpleGenTx
    -- * Crypto
  , SimpleCrypto
  , SimpleStandardCrypto
  , SimpleMockCrypto
    -- * Serialisation
  , encodeSimpleHeader
  , decodeSimpleHeader
  , simpleBlockBinaryInfo
  , simpleBlockHashInfo
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Codec.CBOR.Write as CBOR
import           Codec.Serialise (Serialise (..), serialise)
import           Control.Monad.Except
import qualified Data.Binary.Get as Get
import qualified Data.Binary.Put as Put
import qualified Data.ByteString.Lazy as Lazy
import           Data.FingerTree.Strict (Measured (..))
import           Data.Proxy
import           Data.Typeable
import           Data.Word
import           GHC.Generics (Generic)

import           Cardano.Binary (FromCBOR (..), ToCBOR (..))
import           Cardano.Crypto.Hash
import           Cardano.Prelude (NoUnexpectedThunks (..))

import           Ouroboros.Network.Block

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock.Address
import           Ouroboros.Consensus.Ledger.Mock.State
import qualified Ouroboros.Consensus.Ledger.Mock.UTxO as Mock
import           Ouroboros.Consensus.Mempool.API
import           Ouroboros.Consensus.Util ((.:))
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Storage.ImmutableDB (BinaryInfo (..), HashInfo (..))

{-------------------------------------------------------------------------------
  Definition of a block

  The primed versions allow to vary the @ext@ parameter independently of the
  previous block hash.
-------------------------------------------------------------------------------}

type SimpleBlock  c ext = SimpleBlock'  c ext ext
type SimpleHeader c ext = Header (SimpleBlock c ext)

data SimpleBlock' c ext ext' = SimpleBlock {
      simpleHeader :: Header (SimpleBlock' c ext ext')
    , simpleBody   :: SimpleBody
    }
  deriving stock    (Generic, Show, Eq)
  deriving anyclass (Serialise)

instance GetHeader (SimpleBlock' c ext ext') where
  data Header (SimpleBlock' c ext ext') = SimpleHeader {
        -- | The header hash
        --
        -- This is the hash of the header itself. This is a bit unpleasant,
        -- because it makes the hash look self-referential (when computing the
        -- hash we must ignore the 'simpleHeaderHash' field). However, the benefit
        -- is that we can give a 'HasHeader' instance that does not require
        -- a (static) 'Serialise' instance.
        simpleHeaderHash :: HeaderHash (SimpleBlock' c ext ext')

        -- | Fields required for the 'HasHeader' instance
      , simpleHeaderStd  :: SimpleStdHeader c ext

        -- | Header extension
        --
        -- This extension will be required when using 'SimpleBlock' for specific
        -- consensus protocols.
      , simpleHeaderExt  :: ext'
      }
    deriving (Generic, Show, Eq, NoUnexpectedThunks)

  getHeader = simpleHeader

data SimpleStdHeader c ext = SimpleStdHeader {
      simplePrev      :: ChainHash (SimpleBlock c ext)
    , simpleSlotNo    :: SlotNo
    , simpleBlockNo   :: BlockNo
    , simpleBodyHash  :: Hash (SimpleHash c) SimpleBody
    , simpleBlockSize :: Word64
    }
  deriving stock    (Generic, Show, Eq)
  deriving anyclass (Serialise, NoUnexpectedThunks)

data SimpleBody = SimpleBody {
      simpleTxs :: [Mock.Tx]
    }
  deriving stock    (Generic, Show, Eq)
  deriving anyclass (Serialise)

{-------------------------------------------------------------------------------
  Working with 'SimpleBlock'
-------------------------------------------------------------------------------}

mkSimpleHeader :: SimpleCrypto c
               => (ext' -> CBOR.Encoding)
               -> SimpleStdHeader c ext
               -> ext'
               -> Header (SimpleBlock' c ext ext')
mkSimpleHeader encodeExt std ext =
    headerWithoutHash {
        simpleHeaderHash = hashWithSerialiser
                             (encodeSimpleHeader encodeExt)
                             headerWithoutHash
      }
  where
    headerWithoutHash = SimpleHeader {
        simpleHeaderHash = error "Serialise instances should ignore hash"
      , simpleHeaderStd  = std
      , simpleHeaderExt  = ext
      }

-- | Check whether the block matches the header
matchesSimpleHeader :: SimpleCrypto c
                    => Header (SimpleBlock' c ext ext')
                    -> SimpleBlock'  c ext ext''
                    -> Bool
matchesSimpleHeader SimpleHeader{..} SimpleBlock {..} =
    simpleBodyHash == hash simpleBody
  where
    SimpleStdHeader{..} = simpleHeaderStd

countSimpleGenTxs :: SimpleBlock' c ext ext'' -> Word64
countSimpleGenTxs = fromIntegral . length . simpleTxs . simpleBody

{-------------------------------------------------------------------------------
  HasHeader instance for SimpleHeader
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext) => HasHeader (SimpleHeader c ext) where
  blockHash      =            simpleHeaderHash
  blockPrevHash  = castHash . simplePrev    . simpleHeaderStd
  blockSlot      =            simpleSlotNo  . simpleHeaderStd
  blockNo        =            simpleBlockNo . simpleHeaderStd
  blockInvariant = const True

{-------------------------------------------------------------------------------
  HasHeader instance for SimpleBlock
-------------------------------------------------------------------------------}

type instance HeaderHash (SimpleBlock' c ext ext') =
  Hash (SimpleHash c) (Header (SimpleBlock' c ext ext'))

instance (SimpleCrypto c, Typeable ext)
      => Measured BlockMeasure (SimpleBlock c ext) where
  measure = blockMeasure

instance (SimpleCrypto c, Typeable ext) => HasHeader (SimpleBlock c ext) where
  blockHash      =            blockHash     . simpleHeader
  blockPrevHash  = castHash . blockPrevHash . simpleHeader
  blockSlot      =            blockSlot     . simpleHeader
  blockNo        =            blockNo       . simpleHeader
  blockInvariant = const True

instance (SimpleCrypto c, Typeable ext) => StandardHash (SimpleBlock c ext)

{-------------------------------------------------------------------------------
  HasUTxO instance
-------------------------------------------------------------------------------}

instance Mock.HasUtxo (SimpleBlock' c ext ext') where
  txIns      = Mock.txIns      . simpleBody
  txOuts     = Mock.txOuts     . simpleBody
  confirmed  = Mock.confirmed  . simpleBody
  updateUtxo = Mock.updateUtxo . simpleBody

instance Mock.HasUtxo SimpleBody where
  txIns      = Mock.txIns      . simpleTxs
  txOuts     = Mock.txOuts     . simpleTxs
  confirmed  = Mock.confirmed  . simpleTxs
  updateUtxo = Mock.updateUtxo . simpleTxs

{-------------------------------------------------------------------------------
  Envelope validation
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext) => HasAnnTip (SimpleBlock c ext)
  -- Use defaults

instance (SimpleCrypto c, Typeable ext) => ValidateEnvelope (SimpleBlock c ext)
  -- Use defaults

{-------------------------------------------------------------------------------
  Update the ledger
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext, SupportedBlock (SimpleBlock c ext))
      => UpdateLedger (SimpleBlock c ext) where
  newtype LedgerState (SimpleBlock c ext) = SimpleLedgerState {
        simpleLedgerState :: MockState (SimpleBlock c ext)
      }
    deriving stock   (Generic, Show, Eq)
    deriving newtype (Serialise, NoUnexpectedThunks)

  data LedgerConfig (SimpleBlock c ext) =
      SimpleLedgerConfig
    deriving (Show)

  type LedgerError (SimpleBlock c ext) = MockError (SimpleBlock c ext)

  applyChainTick _ = TickedLedgerState
  applyLedgerBlock _cfg = updateSimpleLedgerState
  reapplyLedgerBlock _cfg = (mustSucceed . runExcept) .: updateSimpleLedgerState
    where
      mustSucceed (Left  err) = error ("reapplyLedgerBlock: unexpected error: " <> show err)
      mustSucceed (Right st)  = st
  ledgerTipPoint (SimpleLedgerState st) = mockTip st

updateSimpleLedgerState :: (SimpleCrypto c, Typeable ext)
                        => SimpleBlock c ext
                        -> LedgerState (SimpleBlock c ext)
                        -> Except (MockError (SimpleBlock c ext))
                                  (LedgerState (SimpleBlock c ext))
updateSimpleLedgerState b (SimpleLedgerState st) =
    SimpleLedgerState <$> updateMockState b st

updateSimpleUTxO :: Mock.HasUtxo a
                 => a
                 -> TickedLedgerState (SimpleBlock c ext)
                 -> Except (MockError (SimpleBlock c ext))
                           (TickedLedgerState (SimpleBlock c ext))
updateSimpleUTxO b (TickedLedgerState slot (SimpleLedgerState st)) =
    TickedLedgerState slot . SimpleLedgerState <$> updateMockUTxO b st

genesisSimpleLedgerState :: AddrDist -> LedgerState (SimpleBlock c ext)
genesisSimpleLedgerState = SimpleLedgerState . genesisMockState

{-------------------------------------------------------------------------------
  Support for the mempool
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext, SupportedBlock (SimpleBlock c ext))
      => ApplyTx (SimpleBlock c ext) where
  data GenTx (SimpleBlock c ext) = SimpleGenTx
    { simpleGenTx   :: !Mock.Tx
    , simpleGenTxId :: !Mock.TxId
    } deriving stock    (Generic, Eq, Ord)
      deriving anyclass (Serialise)

  txSize = fromIntegral . Lazy.length . serialise

  type ApplyTxErr (SimpleBlock c ext) = MockError (SimpleBlock c ext)

  applyTx   = const updateSimpleUTxO
  reapplyTx = const updateSimpleUTxO

instance HasTxId (GenTx (SimpleBlock c ext)) where
  newtype TxId (GenTx (SimpleBlock c ext)) = SimpleGenTxId
    { unSimpleGenTxId :: Mock.TxId
    } deriving stock   (Generic)
      deriving newtype (Show, Eq, Ord, Serialise)

  txId = SimpleGenTxId . simpleGenTxId

instance (Typeable p, Typeable c) => NoUnexpectedThunks (GenTx (SimpleBlock p c)) where
  showTypeOf _ = show $ typeRep (Proxy @(GenTx (SimpleBlock p c)))

instance Mock.HasUtxo (GenTx (SimpleBlock p c)) where
  txIns      = Mock.txIns      . simpleGenTx
  txOuts     = Mock.txOuts     . simpleGenTx
  confirmed  = Mock.confirmed  . simpleGenTx
  updateUtxo = Mock.updateUtxo . simpleGenTx

instance Condense (GenTx (SimpleBlock p c)) where
    condense = condense . simpleGenTx

instance Show (GenTx (SimpleBlock p c)) where
    show = show . simpleGenTx

instance Condense (GenTxId (SimpleBlock p c)) where
    condense = condense . unSimpleGenTxId

mkSimpleGenTx :: Mock.Tx -> GenTx (SimpleBlock c ext)
mkSimpleGenTx tx = SimpleGenTx
    { simpleGenTx   = tx
    , simpleGenTxId = hash tx
    }

{-------------------------------------------------------------------------------
  Support for QueryLedger
-------------------------------------------------------------------------------}

instance (SimpleCrypto c, Typeable ext, SupportedBlock (SimpleBlock c ext))
      => QueryLedger (SimpleBlock c ext) where
  data Query (SimpleBlock c ext) result
    deriving (Show)

  answerQuery = \case {}
  eqQuery     = \case {}

instance ShowQuery (Query (SimpleBlock c ext)) where
  showResult = \case {}

{-------------------------------------------------------------------------------
  Crypto needed for simple blocks
-------------------------------------------------------------------------------}

class (HashAlgorithm (SimpleHash c), Typeable c) => SimpleCrypto c where
  type family SimpleHash c :: *

data SimpleStandardCrypto
data SimpleMockCrypto

instance SimpleCrypto SimpleStandardCrypto where
  type SimpleHash SimpleStandardCrypto = MD5

instance SimpleCrypto SimpleMockCrypto where
  type SimpleHash SimpleMockCrypto = ShortHash

{-------------------------------------------------------------------------------
  Condense instances
-------------------------------------------------------------------------------}

instance Condense ext' => Condense (Header (SimpleBlock' c ext ext')) where
  condense SimpleHeader{..} = mconcat [
        "("
      , condense simplePrev
      , "->"
      , condense simpleHeaderHash
      , ","
      , condense simpleSlotNo
      , ","
      , condense simpleHeaderExt
      , ")"
      ]
    where
      SimpleStdHeader{..} = simpleHeaderStd

instance Condense ext' => Condense (SimpleBlock' c ext ext') where
  condense SimpleBlock{..} = mconcat [
        "("
      , condense simplePrev
      , "->"
      , condense simpleHeaderHash
      , ","
      , condense simpleSlotNo
      , ","
      , condense simpleHeaderExt
      , ","
      , condense simpleTxs
      , ")"
      ]
    where
      SimpleHeader{..}    = simpleHeader
      SimpleStdHeader{..} = simpleHeaderStd
      SimpleBody{..}      = simpleBody

{-------------------------------------------------------------------------------
  Serialisation
-------------------------------------------------------------------------------}

instance ToCBOR SimpleBody where
  toCBOR = encode

encodeSimpleHeader :: (ext' -> CBOR.Encoding)
                   -> Header (SimpleBlock' c ext ext')
                   -> CBOR.Encoding
encodeSimpleHeader encodeExt SimpleHeader{..} =  mconcat [
      CBOR.encodeListLen 2
    , encode simpleHeaderStd
    , encodeExt simpleHeaderExt
    ]

decodeSimpleHeader :: SimpleCrypto c
                   => (ext' -> CBOR.Encoding)
                   -> (forall s. CBOR.Decoder s ext')
                   -> forall s. CBOR.Decoder s (Header (SimpleBlock' c ext ext'))
decodeSimpleHeader encodeExt decodeExt = do
    CBOR.decodeListLenOf 2
    mkSimpleHeader encodeExt <$> decode <*> decodeExt

-- | Custom 'Serialise' instance that doesn't serialise the hash
instance (SimpleCrypto c, Serialise ext')
      => Serialise (Header (SimpleBlock' c ext ext')) where
  encode = encodeSimpleHeader encode
  decode = decodeSimpleHeader encode decode

simpleBlockBinaryInfo :: (SimpleCrypto c, Serialise ext')
                      => SimpleBlock' c ext ext' -> BinaryInfo CBOR.Encoding
simpleBlockBinaryInfo b = BinaryInfo
    { binaryBlob   = encode b
    , headerOffset = 2 -- For the 'encodeListLen'
    , headerSize   = fromIntegral $ Lazy.length $ serialise (getHeader b)
    }

-- | As we can't simply create a 'Hash' from a 'ByteString', we're (ab)using
-- its 'FromCBOR'/'ToCBOR' instances. This means we're adding an extra byte
-- for the CBOR tag.
simpleBlockHashInfo
  :: forall c ext ext'. (SimpleCrypto c, Typeable ext, Typeable ext')
  => HashInfo (HeaderHash (SimpleBlock' c ext ext'))
simpleBlockHashInfo = HashInfo
    { hashSize
    , getHash  = do
        bl <- Get.getLazyByteString (fromIntegral hashSize)
        case CBOR.deserialiseFromBytes fromCBOR bl of
          Left e       -> fail (show e)
          Right (_, h) -> return h
    , putHash  = Put.putByteString . CBOR.toStrictByteString . toCBOR
    }
  where
    -- + 1 For the CBOR tag
    hashSize = 1 + fromIntegral (byteCount (Proxy @(SimpleHash c)))
