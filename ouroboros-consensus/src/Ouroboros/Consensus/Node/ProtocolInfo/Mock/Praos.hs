{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Node.ProtocolInfo.Mock.Praos (
    protocolInfoPraos
  ) where

import           Codec.CBOR.Decoding (decodeListLenOf)
import           Codec.CBOR.Encoding (encodeListLen)
import           Codec.Serialise (Serialise (..))
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import           Cardano.Binary (fromCBOR, toCBOR)
import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract (IsALeaderOrNot (..))
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.Praos

protocolInfoPraos :: NumCoreNodes
                  -> NodeId
                  -> PraosParams
                  -> ProtocolInfo (SimplePraosBlock SimpleMockCrypto
                                                    PraosMockCrypto)
protocolInfoPraos numCoreNodes nid params =
    ProtocolInfo {
        pInfoConfig = EncNodeConfig {
            encNodeConfigP = PraosNodeConfig {
                praosParams        = params
              , praosIsLeader      = case nid of
                  RelayId rid -> IsNotALeader rid
                  CoreId  cid -> IsALeader PraosIsLeader {
                    praosCoreNodeId = cid
                  , praosSignKeyVRF = SignKeyMockVRF $ fromIntegral $ unCoreNodeId cid
                  }
              , praosInitialEta    = 0
              , praosInitialStake  = genesisStakeDist addrDist
              , praosVerKeys       = verKeys
              }
          , encNodeConfigExt = addrDist
          }
      , pInfoInitLedger = ExtLedgerState {
            ledgerState         = genesisSimpleLedgerState addrDist
          , ouroborosChainState = []
          }
      , pInfoInitState = case nid of
          RelayId _rid -> error "non-core node used PraosNodeState"
          CoreId   cid -> PraosNodeState $ SignKeyMockKES
             (fst $ verKeys IntMap.! cidInt) -- key ID
             0                               -- KES initial slot
             (praosLifetimeKES params)       -- KES lifetime
           where
             cidInt = fromIntegral $ unCoreNodeId cid
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

    verKeys :: IntMap (VerKeyKES MockKES, VerKeyVRF MockVRF)
    verKeys = IntMap.fromList
      [ (cidInt, (VerKeyMockKES cidInt, VerKeyMockVRF cidInt))
      | cid <- enumCoreNodes numCoreNodes
      , let cidInt = fromIntegral $ unCoreNodeId cid
      ]

instance Serialise (BlockInfo PraosMockCrypto) where
  encode BlockInfo {..} = mconcat
    [ encodeListLen 3
    , encode biSlot
    , toCBOR biRho
    , encode biStake
    ]
  decode = do
    decodeListLenOf 3
    biSlot  <- decode
    biRho   <- fromCBOR
    biStake <- decode
    return BlockInfo {..}
