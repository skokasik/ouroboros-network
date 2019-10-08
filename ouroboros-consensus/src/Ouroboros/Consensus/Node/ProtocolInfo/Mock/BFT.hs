module Ouroboros.Consensus.Node.ProtocolInfo.Mock.BFT (
    protocolInfoBft
  ) where

import qualified Data.Map as Map

import           Cardano.Crypto.DSIGN

import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Protocol.BFT

protocolInfoBft :: NumCoreNodes
                -> NodeId
                -> SecurityParam
                -> ProtocolInfo (SimpleBftBlock SimpleMockCrypto BftMockCrypto)
protocolInfoBft numCoreNodes nid securityParam =
    ProtocolInfo {
        pInfoConfig = BftNodeConfig {
            bftParams   = BftParams {
                              bftNumCoreNodes  = numCoreNodes
                            , bftSecurityParam = securityParam
                            }
          , bftIsLeader = case nid of
              RelayId rid -> IsNotALeader rid
              CoreId  cid -> IsALeader BftIsLeader {
                  bftCoreNodeId = cid
                , bftSignKey    = SignKeyMockDSIGN (fromIntegral (unCoreNodeId cid))
                }
          , bftVerKeys  = Map.fromList
              [ (cid', VerKeyMockDSIGN (fromIntegral (unCoreNodeId cid')))
              | cid' <- enumCoreNodes numCoreNodes
              ]
          }
      , pInfoInitLedger = ExtLedgerState (genesisSimpleLedgerState addrDist) ()
      , pInfoInitState  = ()
      }
  where
    addrDist :: AddrDist
    addrDist = mkAddrDist numCoreNodes
