-- | Test the Praos chain selection rule but with explicit leader schedule
module Ouroboros.Consensus.Node.ProtocolInfo.Mock.PraosRule (
    protocolInfoPraosRule
  ) where

import           Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import           Cardano.Crypto.KES
import           Cardano.Crypto.VRF

import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Node.ProtocolInfo.Abstract
import           Ouroboros.Consensus.NodeId (CoreNodeId (..), NodeId (..))
import           Ouroboros.Consensus.Protocol.Abstract (IsALeaderOrNot (..))
import           Ouroboros.Consensus.Protocol.LeaderSchedule
import           Ouroboros.Consensus.Protocol.Praos

protocolInfoPraosRule :: NumCoreNodes
                      -> NodeId
                      -> PraosParams
                      -> LeaderSchedule
                      -> ProtocolInfo (SimplePraosRuleBlock SimpleMockCrypto)
protocolInfoPraosRule numCoreNodes
                      nid
                      params
                      schedule =
    ProtocolInfo {
      pInfoConfig    = WLSNodeConfig
        { lsNodeConfigSchedule = schedule
        , lsNodeConfigP        =
            PraosNodeConfig
            { praosParams       = params
            , praosIsLeader     = case nid of
                RelayId rid -> IsNotALeader rid
                CoreId  cid -> IsALeader PraosIsLeader {
                  praosCoreNodeId = cid
                , praosSignKeyVRF = NeverUsedSignKeyVRF
                }
            , praosInitialEta   = 0
            , praosInitialStake = genesisStakeDist addrDist
            , praosVerKeys      = verKeys
            }
        , lsNodeConfigNodeId   = nid
        }
    , pInfoInitLedger = ExtLedgerState
        { ledgerState         = genesisSimpleLedgerState addrDist
        , ouroborosChainState = ()
        }
    , pInfoInitState  = ()
    }
  where
    addrDist = mkAddrDist numCoreNodes

    verKeys :: IntMap (VerKeyKES NeverKES, VerKeyVRF NeverVRF)
    verKeys = IntMap.fromList
      [ (cidInt', (NeverUsedVerKeyKES, NeverUsedVerKeyVRF))
      | cid' <- enumCoreNodes numCoreNodes
      , let cidInt' = fromIntegral $ unCoreNodeId cid'
      ]
