{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Setup network
module Test.Dynamic.Network (
    runNodeNetwork
  , MaxLatencies (..)
  , NodeNetworkArgs (..)
  , TracingConstraints
  , LatencyInjection (..)
    -- * Tracers
  , MiniProtocolExpectedException (..)
  , MiniProtocolFatalException (..)
  , MiniProtocolState (..)
    -- * Test Output
  , TestOutput (..)
  , NodeOutput (..)
  , NodeDBs (..)
  ) where

import qualified Control.Exception as Exn
import           Control.Monad
import           Control.Tracer
import           Crypto.Random (ChaChaDRG, drgNew)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Proxy (Proxy (..))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Typeable as Typeable
import           GHC.Stack
import           System.Random.SplitMix (SMGen)

import           Control.Monad.Class.MonadSTM.Strict as Strict
import           Control.Monad.Class.MonadThrow

import           Network.TypedProtocol.Channel
import           Network.TypedProtocol.Codec (AnyMessage (..))

import           Ouroboros.Network.Block
import           Ouroboros.Network.MockChain.Chain

import qualified Ouroboros.Network.BlockFetch.Client as BFClient
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.PipelineDecision
                     (pipelineDecisionLowHighMark)
import           Ouroboros.Network.Protocol.ChainSync.Type
import           Ouroboros.Network.Protocol.TxSubmission.Type
import qualified Ouroboros.Network.TxSubmission.Inbound as TxInbound
import qualified Ouroboros.Network.TxSubmission.Outbound as TxOutbound

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime
import qualified Ouroboros.Consensus.BlockFetchServer as BFServer
import           Ouroboros.Consensus.ChainSyncClient (ClockSkew (..))
import qualified Ouroboros.Consensus.ChainSyncClient as CSClient
import           Ouroboros.Consensus.ChainSyncServer (Tip)
import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.NodeKernel
import           Ouroboros.Consensus.NodeNetwork
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.ResourceRegistry
import           Ouroboros.Consensus.Util.STM

import qualified Ouroboros.Storage.ChainDB as ChainDB
import           Ouroboros.Storage.ChainDB.Impl (ChainDbArgs (..))
import           Ouroboros.Storage.EpochInfo (EpochInfo, newEpochInfo)
import qualified Ouroboros.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Storage.LedgerDB.DiskPolicy as LgrDB
import qualified Ouroboros.Storage.LedgerDB.InMemory as LgrDB
import qualified Ouroboros.Storage.Util.ErrorHandling as EH

import           Test.Dynamic.TxGen
import           Test.Dynamic.Util.LivePipes
import           Test.Dynamic.Util.NodeJoinPlan
import           Test.Dynamic.Util.NodeTopology
import           Test.Dynamic.Util.OutagesPlan as Outages

import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.FS.Sim.STM (simHasFS)
import           Test.Util.Tracer

data NodeNetworkArgs m blk = NodeNetworkArgs
  { nnaLatencySeed         :: !(LatencyInjection SMGen)
  , nnaLatestReadySlot     :: !(StrictTVar m SlotNo)
    -- ^ Setting this to @s@ indicates that all slots preceding @s@ have
    -- quiesced
  , nnaMaxLatencies        :: !MaxLatencies
  , nnaNodeJoinPlan        :: !NodeJoinPlan
  , nnaNodeTopology        :: !NodeTopology
  , nnaNumCoreNodes        :: !NumCoreNodes
  , nnaOutagesPlan         :: !OutagesPlan
  , nnaProtocol            :: !(CoreNodeId -> ProtocolInfo blk)
  , nnaQuiescenceThreshold :: !DiffTime
    -- ^ A slot ends when there have been no sent-but-not-yet-received mini
    -- protocol messages for at least this long
  , nnaRegistry            :: !(ResourceRegistry m)
  , nnaTestBtime           :: !(TestBlockchainTime m)
  , nnaTxSeed              :: !ChaChaDRG
  }

-- | Setup a network of core nodes, where each joins according to the node join
-- plan and is interconnected according to the node topology
--
-- We run for the specified number of blocks, then return the final state of
-- each node.
runNodeNetwork :: forall m blk.
                    ( IOLike m
                    , RunNode blk
                    , TxGen blk
                    , TracingConstraints blk
                    , HasCallStack
                    )
               => NodeNetworkArgs m blk
               -> m (TestOutput blk)
runNodeNetwork NodeNetworkArgs
  { nnaLatencySeed         = mbInitSMG
  , nnaLatestReadySlot     = latestReadySlot
  , nnaMaxLatencies
  , nnaNodeJoinPlan        = nodeJoinPlan
  , nnaNodeTopology        = nodeTopology
  , nnaNumCoreNodes        = numCoreNodes
  , nnaOutagesPlan
  , nnaProtocol            = pInfo
  , nnaQuiescenceThreshold = quiescenceThreshold
  , nnaRegistry            = registry
  , nnaTestBtime           = testBtime
  , nnaTxSeed              = initRNG
  } = do
    -- This function is organized around the notion of a network of nodes as a
    -- simple graph with no loops. The graph topology is determined by
    -- @nodeTopology@.
    --
    -- Each graph node is a Ouroboros core node, with its own private threads
    -- managing the node's internal state. Some nodes join the network later
    -- than others, according to @nodeJoinPlan@.
    --
    -- Each undirected edge denotes two opposing directed edges. Each directed
    -- edge denotes a bundle of mini protocols with client threads on the tail
    -- node and server threads on the head node. These mini protocols begin as
    -- soon as both nodes have joined the network, according to @nodeJoinPlan@.

    (livePipesVar, downRequestVars) <-
      spawnTickerThread
        coreNodeIds quiescenceThreshold nnaOutagesPlan
        latestReadySlot btime

    varRNG <- uncheckedNewTVarM initRNG

    -- allocate a TMVar for each node's network app
    nodeVars <- fmap Map.fromList $ do
      forM coreNodeIds $ \nid -> (,) nid <$> Strict.newTVarM Nothing

    -- allocate a variable to record 'MiniProtocolExpectedException's
    accMPEEsVar <- Strict.newTVarM Map.empty
    let edgeTracer = nullDebugTracer <> Tracer go
          where
            go = \case
              (eg, s, st@MiniProtocolDelayed, mbE) -> forM_ mbE $ \case
                MPEEPlannedOutage _ mbE' -> go (eg, s, st, mbE')
                _                        -> atomically $ do
                  let upd = Map.insertWith Set.union s (Set.singleton eg)
                  modifyTVar accMPEEsVar upd
              _ -> pure ()

    -- spawn threads for each undirected edge
    mbSMG <- mapM uncheckedNewTVarM mbInitSMG
    let edges = edgesNodeTopology nodeTopology
    forM_ edges $ \edge -> do
      void $ forkLinkedThread registry $ do
        undirectedEdge edgeTracer mbSMG livePipesVar downRequestVars nodeVars edge

    -- create nodes
    let nodesByJoinSlot =
          List.sortOn fst $   -- sort non-descending by join slot
          map (\nv@(n, _) -> (joinSlotOf n, nv)) $
          Map.toList nodeVars
    nodes <- forM nodesByJoinSlot $ \(joinSlot, (coreNodeId, nodeVar)) -> do
      -- do not start the node before its joinSlot
      tooLate <- blockUntilSlot btime joinSlot
      when tooLate $ do
        error $ "unsatisfiable nodeJoinPlan: " ++ show coreNodeId

      -- allocate the node's internal state and spawn its internal threads
      (node, readNodeInfo, app) <- createNode varRNG coreNodeId

      -- unblock the threads of edges that involve this node
      atomically $ writeTVar nodeVar (Just app)

      return (coreNodeId, pInfoConfig (pInfo coreNodeId), node, readNodeInfo)

    -- Wait some extra time after the end of the test block fetch and chain
    -- sync to finish
    testBlockchainTimeDone testBtime
    threadDelay 2000   -- arbitrary "small" duration

    -- Close the 'ResourceRegistry': this shuts down the background threads of
    -- a node. This is important because we close the ChainDBs in
    -- 'getTestOutput' and if background threads that use the ChainDB are
    -- still running at that point, they will throw a 'CloseDBError'.
    closeRegistry registry

    accMPEEs <- atomically $ readTVar accMPEEsVar
    getTestOutput nodes accMPEEs
  where
    btime = testBlockchainTime testBtime

    coreNodeIds :: [CoreNodeId]
    coreNodeIds = enumCoreNodes numCoreNodes

    joinSlotOf :: CoreNodeId -> SlotNo
    joinSlotOf = coreNodeIdJoinSlot nodeJoinPlan

    undirectedEdge ::
         HasCallStack
      => Tracer m (OutageEdge, SlotNo, MiniProtocolState, Maybe (MiniProtocolExpectedException blk))
      -> LatencyInjection (StrictTVar m SMGen)
      -> LivePipesVar m
      -> Map OutageEdge (DownRequestVar m)
      -> Map CoreNodeId (StrictTVar m (Maybe (LimitedApp m NodeId blk)))
      -> (CoreNodeId, CoreNodeId)
      -> m ()
    undirectedEdge tr mbSMG livePipesVar downRequestVars nodeVars (node1, node2) = do
        let lookupDRV n1 n2 = case Map.lookup (n1, n2) downRequestVars of
              Nothing  -> error $ "down request variable not found: " ++ show (n1, n2)
              Just drv -> drv
            drv12 = lookupDRV node1 node2
            drv21 = lookupDRV node2 node1

        let lookupNodeVar node = case Map.lookup node nodeVars of
              Nothing -> error $ "node not found: " ++ show node
              Just nv -> nv
            nv1 = lookupNodeVar node1
            nv2 = lookupNodeVar node2

        let activeWaiting ::
                 SlotNo -> SlotNo
              -> m ( SlotNo
                   , SlotNo
                   , (CoreNodeId, LimitedApp m NodeId blk)
                   , (CoreNodeId, LimitedApp m NodeId blk)
                   )
            activeWaiting s12 s21 = do
              x <- atomically $ do
                mbS12 <- readTVar drv12
                mbS21 <- readTVar drv21
                case (mbS12, mbS21) of
                  (Nothing, Nothing) -> do
                    -- block until both nodes have joined the network
                    mbEndpoint1 <- readTVar nv1
                    mbEndpoint2 <- readTVar nv2
                    case (,) <$> mbEndpoint1 <*> mbEndpoint2 of
                      Nothing         -> retry
                      Just (ep1, ep2) ->
                        pure $ Right (s12, s21, (node1, ep1), (node2, ep2))
                  _ -> do
                    forM_ mbS12 $ \_ -> writeTVar drv12 Nothing
                    forM_ mbS21 $ \_ -> writeTVar drv21 Nothing
                    pure $ Left (fromMaybe s12 mbS12, fromMaybe s21 mbS21)
              case x of
                Left (s12', s21') -> activeWaiting s12' s21'
                Right tup         -> pure tup

        (s12, s21, endpoint1, endpoint2) <- activeWaiting (SlotNo 0) (SlotNo 0)

        -- spawn threads for both directed edges
        let de drv downUntil ep1@(n1, _) ep2@(n2, _) =
                directedEdge
                  nnaMaxLatencies tr' btime mbSMG
                  livePipesVar drv downUntil
                  ep1 ep2
              where
                tr' = contramap (\(s, st, mbE) -> ((n1, n2), s, st, mbE)) tr
        void $ withAsyncsWaitAny2
            (de drv12 s12 endpoint1 endpoint2)
            (de drv21 s21 endpoint2 endpoint1)

      where
        withAsyncsWaitAny2 x1 x2 = withAsyncsWaitAny $ x1 NE.:| [x2]


    -- | Produce transactions every time the slot changes and submit them to
    -- the mempool.
    txProducer :: HasCallStack
               => NodeConfig (BlockProtocol blk)
               -> m ChaChaDRG
                  -- ^ How to get a DRG
               -> STM m (ExtLedgerState blk)
                  -- ^ How to get the current ledger state
               -> Mempool m blk TicketNo
               -> m ()
    txProducer cfg produceDRG getExtLedger mempool =
      onSlotChange btime $ \_curSlotNo -> do
        varDRG <- uncheckedNewTVarM =<< produceDRG
        txs <- atomically $ do
          ledger <- ledgerState <$> getExtLedger
          simChaChaT varDRG id $ testGenTxs numCoreNodes cfg ledger
        void $ addTxs mempool txs

    mkArgs :: NodeConfig (BlockProtocol blk)
           -> ExtLedgerState blk
           -> EpochInfo m
           -> Tracer m (Point blk)
              -- ^ invalid block tracer
           -> Tracer m (Point blk, BlockNo)
              -- ^ added block tracer
           -> NodeDBs (StrictTVar m MockFS)
           -> ChainDbArgs m blk
    mkArgs
      cfg initLedger epochInfo
      invalidTracer addTracer
      nodeDBs = ChainDbArgs
        { -- Decoders
          cdbDecodeHash       = nodeDecodeHeaderHash (Proxy @blk)
        , cdbDecodeBlock      = nodeDecodeBlock cfg
        , cdbDecodeLedger     = nodeDecodeLedgerState cfg
        , cdbDecodeChainState = nodeDecodeChainState (Proxy @blk)
          -- Encoders
        , cdbEncodeBlock      = nodeEncodeBlock cfg
        , cdbEncodeHash       = nodeEncodeHeaderHash (Proxy @blk)
        , cdbEncodeLedger     = nodeEncodeLedgerState cfg
        , cdbEncodeChainState = nodeEncodeChainState (Proxy @blk)
          -- Error handling
        , cdbErrImmDb         = EH.monadCatch
        , cdbErrVolDb         = EH.monadCatch
        , cdbErrVolDbSTM      = EH.throwSTM
          -- HasFS instances
        , cdbHasFSImmDb       = simHasFS EH.monadCatch (nodeDBsImm nodeDBs)
        , cdbHasFSVolDb       = simHasFS EH.monadCatch (nodeDBsVol nodeDBs)
        , cdbHasFSLgrDB       = simHasFS EH.monadCatch (nodeDBsLgr nodeDBs)
          -- Policy
        , cdbValidation       = ImmDB.ValidateAllEpochs
        , cdbBlocksPerFile    = 4
        , cdbParamsLgrDB      = LgrDB.ledgerDbDefaultParams (protocolSecurityParam cfg)
        , cdbDiskPolicy       = LgrDB.defaultDiskPolicy (protocolSecurityParam cfg) generousApproxSlotLen
          -- Integration
        , cdbNodeConfig       = cfg
        , cdbEpochInfo        = epochInfo
        , cdbIsEBB            = \blk -> if nodeIsEBB blk
                                        then Just (blockHash blk)
                                        else Nothing
        , cdbGenesis          = return initLedger
        -- Misc
        , cdbTracer           = Tracer $ \case
              ChainDB.TraceAddBlockEvent
                  (ChainDB.AddBlockValidation ChainDB.InvalidBlock
                      { _invalidPoint = p })
                  -> traceWith invalidTracer p
              ChainDB.TraceAddBlockEvent
                  (ChainDB.AddedBlockToVolDB p bno IsNotEBB)
                  -> traceWith addTracer (p, bno)
              _   -> pure ()
        , cdbRegistry         = registry
        , cdbGcDelay          = 0
        }
      where
        -- a rough estimate of the average slot length, more likely an
        -- overestimate than an underestimate
        generousApproxSlotLen = 100 * (sendL + recvL)
          where
            MaxLatencies sendL recvL = nnaMaxLatencies

    createNode
      :: HasCallStack
      => StrictTVar m ChaChaDRG
      -> CoreNodeId
      -> m ( NodeKernel m NodeId blk
           , m (NodeInfo blk MockFS [])
           , LimitedApp m NodeId blk
           )
    createNode varRNG coreNodeId = do
      let ProtocolInfo{..} = pInfo coreNodeId

      let callbacks :: NodeCallbacks m blk
          callbacks = NodeCallbacks {
              produceBlock = \proof _l slot prevPoint prevNo txs -> do
                let curNo :: BlockNo
                    curNo = succ prevNo

                let prevHash :: ChainHash blk
                    prevHash = castHash (pointHash prevPoint)

                nodeForgeBlock pInfoConfig
                               slot
                               curNo
                               prevHash
                               txs
                               proof

            , produceDRG      = atomically $ simChaChaT varRNG id $ drgNew
            }

      (nodeInfo, readNodeInfo) <- newNodeInfo
      let NodeInfo
            { nodeInfoEvents
            , nodeInfoDBs
            } = nodeInfo

      epochInfo <- newEpochInfo $ nodeEpochSize (Proxy @blk) pInfoConfig
      chainDB <- ChainDB.openDB $ mkArgs
          pInfoConfig pInfoInitLedger epochInfo
          (nodeEventsInvalids nodeInfoEvents)
          (Tracer $ \(p, bno) -> do
              s <- atomically $ getCurrentSlot btime
              traceWith (nodeEventsAdds nodeInfoEvents) (s, p, bno))
          nodeInfoDBs

      let nodeArgs = NodeArgs
            { tracers             = nullDebugTracers
                { forgeTracer = nodeEventsForges nodeInfoEvents
                }
            , registry            = registry
            , maxClockSkew        = ClockSkew 1
            , cfg                 = pInfoConfig
            , initState           = pInfoInitState
            , btime
            , chainDB
            , callbacks
            , blockFetchSize      = nodeBlockFetchSize
            , blockMatchesHeader  = nodeBlockMatchesHeader
            , maxUnackTxs         = 1000 -- TODO
            , mempoolCap          = MempoolCapacity 10 -- TODO
            , chainSyncPipelining = pipelineDecisionLowHighMark 2 4
            }

      nodeKernel <- initNodeKernel nodeArgs
      let app = consensusNetworkApps
                  nodeKernel
                  nullDebugProtocolTracers
                  protocolCodecsId
                  (protocolHandlers nodeArgs nodeKernel)

      void $ forkLinkedThread registry $ do
        -- TODO We assume this effectively runs before anything else in the
        -- slot. With such a short transaction (read one TVar) this is likely
        -- but not necessarily certain.
        onSlotChange btime $ \s -> do
          x <- atomically $ (,)
            <$> ChainDB.getTipBlockNo chainDB
            <*> ChainDB.getTipPoint chainDB
          traceWith (nodeEventsOnsetTips nodeInfoEvents) (s, x)

      void $ forkLinkedThread registry $ txProducer
        pInfoConfig
        (produceDRG callbacks)
        (ChainDB.getCurrentLedger chainDB)
        (getMempool nodeKernel)

      return (nodeKernel, readNodeInfo, LimitedApp app)

{-------------------------------------------------------------------------------
  Running the Mini Protocols on an Ordered Pair of Nodes
-------------------------------------------------------------------------------}

-- | Spawn all mini protocols' threads for a given directed edge in the node
-- network topology (ie an ordered pair of core nodes, client first, server
-- second)
--
-- Key property: if any client thread or server thread in any of the mini
-- protocols throws an exception, restart all of the threads.
--
-- The actual node implementation kills the other threads on the same peer as
-- the thread that threw the exception, and then relies on TCP socket semantics
-- to eventually kill the corresponding threads on the remote peer. The client
-- node recreates its client threads after a delay, and they reconnect to the
-- remote peer, thereby recreating the server threads.
--
-- This mock network instead ensures the property directly via the async
-- interface rather than relying on some sort of mock socket semantics to
-- convey the cancellation.
--
-- It only catches-and-restarts on /expected/ exceptions; anything else will
-- tear down the whole hierarchy of test threads. See
-- 'MiniProtocolExpectedException'.
directedEdge ::
  forall m blk. (IOLike m, SupportedBlock blk)
  => MaxLatencies
  -> Tracer m (SlotNo, MiniProtocolState, Maybe (MiniProtocolExpectedException blk))
  -> BlockchainTime m
  -> LatencyInjection (StrictTVar m SMGen)
  -> LivePipesVar m
  -> DownRequestVar m
  -> SlotNo
  -> (CoreNodeId, LimitedApp m NodeId blk)
  -> (CoreNodeId, LimitedApp m NodeId blk)
  -> m ()
directedEdge maxLatencies tr btime mbSMG livePipesVar drv s0 nodeapp1 nodeapp2 =
    downState s0 Nothing
  where
    -- down, delaying until the next time the edge should be up
    --
    -- respects and clears the given down request
    --
    downState s mbE = do
        redelayed <- atomically $ readTVar drv >>= \case
          Just s' -> Just s' <$ writeTVar drv Nothing
          Nothing -> do
            getCurrentSlot btime >>= check . (>= s)
            pure Nothing

        -- e.g. a down request was set /after/ the protocols already crashed
        --
        case redelayed of
          Just s' -> downState s' $ Just (MPEEPlannedOutage s' mbE)
          Nothing -> do
            traceWith tr (s, MiniProtocolRestarting, mbE)
            upState

    -- up, running until it crashes or the down request is set (both of which
    -- cause exceptions)
    --
    upState = do
        (pids, edge) <-
          directedEdgeInner maxLatencies mbSMG livePipesVar drv nodeapp1 nodeapp2
        edge
          `catch` hExpected pids
          `catch` hUnexpected
      where
        -- Catch expected exceptions and schedule a restart
        --
        hExpected :: Set PipeId -> MiniProtocolExpectedException blk -> m ()
        hExpected pids e = do
          atomically $ mapM_ (forgetLivePipeSTM livePipesVar) pids

          s <- atomically $ getCurrentSlot btime
          traceWith tr (s, MiniProtocolDelayed, Just e)

          -- when to try restarting (may delayed if a subsequent down request
          -- is made)
          --
          let s' = case e of
                MPEEPlannedOutage x _ -> x
                _                     -> succ s
          downState s' (Just e)

        -- Wrap synchronous exceptions in 'MiniProtocolFatalException'
        --
        hUnexpected :: SomeException -> m ()
        hUnexpected e@(Exn.SomeException e') = case fromException e of
          Just (_ :: Exn.AsyncException) -> throwM e
          Nothing                        -> throwM MiniProtocolFatalException
            { mpfeType = Typeable.typeOf e'
            , mpfeExn = e
            }

-- | Spawn threads for all of the mini protocols
--
-- See 'directedEdge'.
directedEdgeInner ::
  forall m blk. (IOLike m, SupportedBlock blk)
  => MaxLatencies
  -> LatencyInjection (StrictTVar m SMGen)
  -> LivePipesVar m
  -> DownRequestVar m
  -> (CoreNodeId, LimitedApp m NodeId blk)
     -- ^ client threads on this node
  -> (CoreNodeId, LimitedApp m NodeId blk)
     -- ^ server threads on this node
  -> m (Set PipeId, m ())
directedEdgeInner maxLatencies mbSMG livePipesVar drv (node1, LimitedApp app1) (node2, LimitedApp app2) = do
    mps <- sequence $
      [ miniProtocol
          (wrapMPEE MPEEChainSyncClient naChainSyncClient)
          naChainSyncServer
      , miniProtocol
          (wrapMPEE MPEEBlockFetchClient naBlockFetchClient)
          (wrapMPEE MPEEBlockFetchServer naBlockFetchServer)
      , miniProtocol
          (wrapMPEE MPEETxSubmissionClient naTxSubmissionClient)
          (wrapMPEE MPEETxSubmissionServer naTxSubmissionServer)
      ]
    let pids = foldMap (\(x,y) -> Set.insert x (Set.singleton y)) $ fmap fst mps
        edge = void $ withAsyncsWaitAny $ (downer NE.:|) $ flattenPairs $ fmap snd mps
    pure (pids, edge)
  where
    flattenPairs :: forall a. [(a, a)] -> [a]
    flattenPairs = uncurry (<>) . unzip

    -- see 'DownRequestVar'
    downer :: m ()
    downer = do
      s' <- atomically $ readTVar drv >>= \case
        Nothing -> retry
        Just s' -> pure s'
      throwM (MPEEPlannedOutage s' Nothing :: MiniProtocolExpectedException blk)

    miniProtocol ::
         (forall unused1 unused2.
            LimitedApp' m NodeId blk unused1 unused2
         -> NodeId
         -> Channel m msg
         -> m ())
        -- ^ client action to run on node1
      -> (forall unused1 unused2.
            LimitedApp' m NodeId blk unused1 unused2
         -> NodeId
         -> Channel m msg
         -> m ())
         -- ^ server action to run on node2
      -> m ((PipeId, PipeId), (m (), m ()))
    miniProtocol client server = do
       (pid1, Pipe pipe1) <- newLivePipe maxLatencies mbSMG livePipesVar
       (pid2, Pipe pipe2) <- newLivePipe maxLatencies mbSMG livePipesVar
       let chan1 = Channel{send = send pipe1, recv = recv pipe2}
           chan2 = Channel{send = send pipe2, recv = recv pipe1}
       pure $ (,)
         (pid1, pid2)
         ( client app1 (fromCoreNodeId node2) chan1
         , server app2 (fromCoreNodeId node1) chan2
         )

    wrapMPEE ::
         Exception e
      => (e -> MiniProtocolExpectedException blk)
      -> (app -> peer -> chan -> m a)
      -> (app -> peer -> chan -> m a)
    wrapMPEE f m = \app them chan ->
        catch (m app them chan) $ throwM . f

{-------------------------------------------------------------------------------
  Node Info
-------------------------------------------------------------------------------}

data NodeInfo blk db ev = NodeInfo
  { nodeInfoEvents :: NodeEvents blk ev
  , nodeInfoDBs    :: NodeDBs db
  }

-- | A vector with an @ev@-shaped element for a particular set of
-- instrumentation events
--
-- The @ev@ type parameter is instantiated by this module at types for
-- 'Tracer's and lists: actions for accumulating and lists as accumulations.
data NodeEvents blk ev = NodeEvents
  { nodeEventsAdds      :: ev (SlotNo, Point blk, BlockNo)
    -- ^ every 'AddedBlockToVolDB' excluding EBBs
  , nodeEventsForges    :: ev (TraceForgeEvent blk)
    -- ^ every 'TraceForgeEvent'
  , nodeEventsInvalids  :: ev (Point blk)
    -- ^ the point of every 'ChainDB.InvalidBlock' event
  , nodeEventsOnsetTips :: ev (SlotNo, (BlockNo, Point blk))
    -- ^ data from 'ChainDB.getTipBlock' for each node at the onset of each
    -- slot
  }

-- | A vector with an element for each database of a node
--
-- The @db@ type parameter is instantiated by this module at types for mock
-- filesystems; either the 'MockFS' type or reference cells thereof.
data NodeDBs db = NodeDBs
  { nodeDBsImm :: db
  , nodeDBsVol :: db
  , nodeDBsLgr :: db
  }

newNodeInfo ::
  forall blk m.
     IOLike m
  => m ( NodeInfo blk (StrictTVar m MockFS) (Tracer m)
       , m (NodeInfo blk MockFS [])
       )
newNodeInfo = do
  (nodeInfoEvents, readEvents) <- do
      (t1, m1) <- recordingTracerTVar
      (t2, m2) <- recordingTracerTVar
      (t3, m3) <- recordingTracerTVar
      (t4, m4) <- recordingTracerTVar
      pure
          ( NodeEvents     t1     t2     t3     t4
          , NodeEvents <$> m1 <*> m2 <*> m3 <*> m4
          )

  (nodeInfoDBs, readDBs) <- do
      let mk :: m (StrictTVar m MockFS, STM m MockFS)
          mk = do
              v <- uncheckedNewTVarM Mock.empty
              pure (v, readTVar v)
      (v1, m1) <- mk
      (v2, m2) <- mk
      (v3, m3) <- mk
      pure
          ( NodeDBs     v1     v2     v3
          , NodeDBs <$> m1 <*> m2 <*> m3
          )

  pure
      ( NodeInfo{nodeInfoEvents, nodeInfoDBs}
      , NodeInfo <$> readEvents <*> atomically readDBs
      )

{-------------------------------------------------------------------------------
  Test Output - output data about each node
-------------------------------------------------------------------------------}

data NodeOutput blk = NodeOutput
  { nodeOutputAdds       :: Map SlotNo (Set (Point blk, BlockNo))
  , nodeOutputCfg        :: NodeConfig (BlockProtocol blk)
  , nodeOutputFinalChain :: Chain blk
  , nodeOutputNodeDBs    :: NodeDBs MockFS
  , nodeOutputForges     :: Map SlotNo blk
  , nodeOutputInvalids   :: Set (Point blk)
  }

data TestOutput blk = TestOutput
  { testOutputMPEEs     :: Map SlotNo (Set OutageEdge)
  , testOutputNodes     :: Map NodeId (NodeOutput blk)
  , testOutputOnsetTips :: Map SlotNo (Map NodeId (BlockNo, Point blk))
  }

-- | Gather the test output from the nodes
getTestOutput ::
    forall m blk. (IOLike m, HasHeader blk)
    => [( CoreNodeId
        , NodeConfig (BlockProtocol blk)
        , NodeKernel m NodeId blk
        , m (NodeInfo blk MockFS [])
        )]
    -> Map SlotNo (Set OutageEdge)
    -> m (TestOutput blk)
getTestOutput nodes testOutputMPEEs = do
    (nodeOutputs', tipBlockNos') <- fmap unzip $ forM nodes $
      \(cid, cfg, node, readNodeInfo) -> do
        let nid = fromCoreNodeId cid
        let chainDB = getChainDB node
        ch <- ChainDB.toChain chainDB
        ChainDB.closeDB chainDB
        nodeInfo <- readNodeInfo
        let NodeInfo
              { nodeInfoEvents
              , nodeInfoDBs
              } = nodeInfo
        let NodeEvents
              { nodeEventsAdds
              , nodeEventsForges
              , nodeEventsInvalids
              , nodeEventsOnsetTips
              } = nodeInfoEvents
        let nodeOutput = NodeOutput
              { nodeOutputAdds       =
                  Map.fromListWith Set.union $
                  [ (s, Set.singleton (p, bno)) | (s, p, bno) <- nodeEventsAdds ]
              , nodeOutputCfg        = cfg
              , nodeOutputFinalChain = ch
              , nodeOutputNodeDBs    = nodeInfoDBs
              , nodeOutputForges     =
                  Map.fromList $
                  [ (s, b) | TraceForgeEvent s b <- nodeEventsForges ]
              , nodeOutputInvalids   = Set.fromList nodeEventsInvalids
              }

        pure
          ( Map.singleton nid nodeOutput
          , Map.singleton nid <$> Map.fromList nodeEventsOnsetTips
          )

    pure $ TestOutput
        { testOutputMPEEs
        , testOutputNodes     = Map.unions nodeOutputs'
        , testOutputOnsetTips = Map.unionsWith Map.union tipBlockNos'
        }

{-------------------------------------------------------------------------------
  Constraints needed for verbose tracing
-------------------------------------------------------------------------------}

nullDebugTracer :: (Applicative m, Show a) => Tracer m a
nullDebugTracer = nullTracer `asTypeOf` showTracing debugTracer

nullDebugTracers ::
     ( Monad m
     , Show peer
     , SupportedBlock blk
     , TracingConstraints blk
     )
  => Tracers m peer blk
nullDebugTracers = nullTracers `asTypeOf` showTracers debugTracer

nullDebugProtocolTracers ::
     ( Monad m
     , HasHeader blk
     , TracingConstraints blk
     , Show peer
     , Show failure
     )
  => ProtocolTracers m peer blk failure
nullDebugProtocolTracers =
  nullProtocolTracers `asTypeOf` showProtocolTracers debugTracer

-- These constraints are when using @showTracer(s) debugTracer@ instead of
-- @nullTracer(s)@.
type TracingConstraints blk =
  ( Show blk
  , Show (ApplyTxErr blk)
  , Show (Header blk)
  , Show (GenTx blk)
  , Show (GenTxId blk)
  )

{-------------------------------------------------------------------------------
  Ancillaries
-------------------------------------------------------------------------------}

-- | Spawn multiple async actions and wait for the first one to complete.
--
-- Each child thread is spawned with 'withAsync' and so won't outlive this one.
-- In the use case where each child thread only terminates on an exception, the
-- 'waitAny' ensures that this parent thread will run until a child terminates
-- with an exception, and it will also reraise that exception.
--
-- Why 'NE.NonEmpty'? An empty argument list would have blocked indefinitely,
-- which is likely not intended.
withAsyncsWaitAny :: IOLike m => NE.NonEmpty (m a) -> m a
withAsyncsWaitAny = go [] . NE.toList
  where
    go acc = \case
      []   -> snd <$> waitAny acc
      m:ms -> withAsync m $ \h -> go (h:acc) ms

-- | The partially instantiation of the 'NetworkApplication' type according to
-- its use in this module
--
-- Used internal to this module, essentially as an abbreviatiation.
data LimitedApp m peer blk =
   forall unused1 unused2.
   LimitedApp (LimitedApp' m peer blk unused1 unused2)

-- | Argument of 'LimitedApp' data constructor
--
-- Used internal to this module, essentially as an abbreviatiation.
type LimitedApp' m peer blk unused1 unused2 =
    NetworkApplication m peer
        (AnyMessage (ChainSync (Header blk) (Tip blk)))
        (AnyMessage (BlockFetch blk))
        (AnyMessage (TxSubmission (GenTxId blk) (GenTx blk)))
        unused1 -- the local node-to-client channel types
        unused2
        ()

{-------------------------------------------------------------------------------
  Tracing
-------------------------------------------------------------------------------}

-- | Non-fatal exceptions expected from the threads of a 'directedEdge'
--
data MiniProtocolExpectedException blk
  = MPEEChainSyncClient (CSClient.ChainSyncClientException blk (Tip blk))
    -- ^ see "Ouroboros.Consensus.ChainSyncClient"
    --
    -- NOTE: the second type in 'ChainSyncClientException' denotes the 'tip'.
    -- If it does not agree with the consensus client & server, 'Dynamic chain
    -- generation' tests will fail, since they will not catch the right
    -- exception.
  | MPEEBlockFetchClient BFClient.BlockFetchProtocolFailure
    -- ^ see "Ouroboros.Network.BlockFetch.Client"
  | MPEEBlockFetchServer (BFServer.BlockFetchServerException blk)
    -- ^ see "Ouroboros.Consensus.BlockFetchServer"
  | MPEETxSubmissionClient TxOutbound.TxSubmissionProtocolError
    -- ^ see "Ouroboros.Network.TxSubmission.Outbound"
  | MPEETxSubmissionServer TxInbound.TxSubmissionProtocolError
    -- ^ see "Ouroboros.Network.TxSubmission.Inbound"
  | MPEEPlannedOutage SlotNo (Maybe (MiniProtocolExpectedException blk))
    -- ^ see 'DownRequestVar'
    --
    -- The second argument is always @Nothing@ when this exception is raised.
    -- However, this data type also occurs in tracing messages, in which case a
    -- @Just@ indicates that the included exception crashed the protocols
    -- before the down request was handled.
  deriving (Show)

instance (SupportedBlock blk) => Exception (MiniProtocolExpectedException blk)

data MiniProtocolState = MiniProtocolDelayed | MiniProtocolRestarting
  deriving (Show)

-- | Any synchronous exception from a 'directedEdge' that was not handled as a
-- 'MiniProtocolExpectedException'
--
data MiniProtocolFatalException = MiniProtocolFatalException
  { mpfeType :: !Typeable.TypeRep
    -- ^ Including the type explicitly makes it easier for a human to debug
  , mpfeExn  :: !SomeException
  }
  deriving (Show)

instance Exception MiniProtocolFatalException

{-------------------------------------------------------------------------------
  Ticker Thread
-------------------------------------------------------------------------------}

-- | A \"bit\" that carries some information when set
--
-- Setting the request during a slot indicates that the corresponding directed
-- edge should enter its down state (i.e. kill its threads) during the slot.
-- When set, it also specifies when the directed edge should restart.
--
-- Each direct edge includes a thread whose only purpose is to raise a
-- 'MPEEPlannedOutage' exception when the corresponding down request is set.
-- This ensures that the direct edge's mini protocols go down during a slot in
-- which the down request is set. Note that the protocols may have crashed on
-- their own before the down request was set (e.g. due to some other exception
-- like @ForkTooDeep@).
--
-- Only the ticker thread sets down requests. And only the corresponding direct
-- edge clears its down request.
--
type DownRequestVar m = StrictTVar m (Maybe SlotNo)

-- | The /ticker thread/
--
-- This thread advances the slot only once all live pipes have been empty for
-- at least the @quiescenceThreshold@ duration. It also sets down requests
-- according to the 'OutagesPlan' before starting the slot.
--
-- See 'DownRequestVar'.
spawnTickerThread ::
  forall m.
     IOLike m
  => [CoreNodeId]
     -- ^ all of them
  -> DiffTime
     -- 'nnaQuiescenceThreshold'
  -> OutagesPlan
  -> StrictTVar m SlotNo
     -- 'nnaLatestReadySlot'
  -> BlockchainTime m
  -> m (LivePipesVar m, Map OutageEdge (DownRequestVar m))
spawnTickerThread
  coreNodeIds quiescenceThreshold plan0
  latestReadySlot btime = do

    livePipesVar <- uncheckedNewTVarM LivePipes
      { nextPipeId = PipeId 0
      , livePipes  = Map.empty
      }

    downRequestVars <-
      fmap Map.fromList $
      sequence $
      [ (,) (n1, n2) <$> uncheckedNewTVarM Nothing
      | n1 <- coreNodeIds
      , n2 <- coreNodeIds
      , n1 /= n2
      ]

    do
      outagesVar <- uncheckedNewTVarM plan0
      onSlotChange btime $ \s -> do
        -- wait until this slot quiesces
        blockUntilQuiescent livePipesVar quiescenceThreshold

        let nextSlot = succ s

        -- down any directed edges that begin an outage in the next slot
        plan <- atomically $ readTVar outagesVar
        forM_ (Outages.nextSlotView plan) $ \(s1, s2s, plan') -> do
          when (nextSlot == s1) $ do
            atomically $ writeTVar outagesVar plan'

            forM_ (Map.toList s2s) $ \(e, s2) -> do
              case Map.lookup e downRequestVars of
                Nothing  -> error "bad outage plan"
                Just drv -> do
                  atomically $ writeTVar drv (Just s2)
                  atomically $ readTVar drv >>= \case
                    Nothing -> pure ()
                    Just{}  -> retry

        -- let the next slot start
        atomically $ writeTVar latestReadySlot nextSlot

    pure (livePipesVar, downRequestVars)
