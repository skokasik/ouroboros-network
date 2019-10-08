{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Node.ProtocolInfo.Mock.PBFT (
    protocolInfoMockPBFT
  ) where

import           Codec.Serialise (Serialise (..))
import qualified Data.Bimap as Bimap

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract (IsALeaderOrNot (..))
import           Ouroboros.Consensus.Protocol.ExtNodeConfig
import           Ouroboros.Consensus.Protocol.PBFT
import qualified Ouroboros.Consensus.Protocol.PBFT.ChainState as CS

protocolInfoMockPBFT :: NumCoreNodes
                     -> NodeId
                     -> PBftParams
                     -> ProtocolInfo (SimplePBftBlock SimpleMockCrypto
                                                      PBftMockCrypto)
protocolInfoMockPBFT numCoreNodes nid params =
    ProtocolInfo {
        pInfoConfig = EncNodeConfig {
            encNodeConfigP = PBftNodeConfig {
                pbftParams   = params { pbftNumCoreNodes =  numCoreNodes }
              , pbftIsLeader = case nid of
                  RelayId rid -> IsNotALeader rid
                  CoreId  cid -> IsALeader PBftIsLeader {
                        pbftCoreNodeId = cid
                      , pbftSignKey    = SignKeyMockDSIGN cidInt
                        -- For Mock PBFT, we use our key as the genesis key.
                      , pbftDlgCert    = (VerKeyMockDSIGN cidInt, VerKeyMockDSIGN cidInt)
                      }
                    where
                      cidInt = fromIntegral $ unCoreNodeId cid
              }
            , encNodeConfigExt = PBftLedgerView $ Bimap.fromList
                [ (VerKeyMockDSIGN cidInt, VerKeyMockDSIGN cidInt)
                | cid <- enumCoreNodes numCoreNodes
                , let cidInt = fromIntegral $ unCoreNodeId cid
                ]
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist)
                                         CS.empty
      , pInfoInitState  = ()
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes

instance Serialise (VerKeyDSIGN MockDSIGN) where
  encode = encodeVerKeyDSIGN
  decode = decodeVerKeyDSIGN
