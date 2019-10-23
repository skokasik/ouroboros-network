{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Dynamic.General (
    prop_general
  , runTestNetwork
    -- * TestConfig
  , TestConfig (..)
  , genLatencySeed
  , genTestConfig
  , noLatencySeed
  , shrinkLatencySeed
  , shrinkTestConfig
    -- * Re-exports
  , TestOutput (..)
  ) where

import           Control.Monad (guard, join)
import           Data.Function (on)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Stack (HasCallStack)
import qualified System.Random.SplitMix as SM
import           Test.QuickCheck

import           Control.Monad.IOSim (runSimOrThrow)

import           Ouroboros.Network.Block (BlockNo (..), pattern BlockPoint,
                     ChainHash (..), pattern GenesisPoint, HasHeader,
                     HeaderHash, Point, pointHash)
import           Ouroboros.Network.MockChain.Chain (Chain)
import qualified Ouroboros.Network.MockChain.Chain as Chain

import           Ouroboros.Consensus.BlockchainTime
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.Run
import           Ouroboros.Consensus.NodeId
import           Ouroboros.Consensus.Protocol (LeaderSchedule (..))
import           Ouroboros.Consensus.Protocol.Abstract (SecurityParam (..))

import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Consensus.Util.Random
import           Ouroboros.Consensus.Util.ResourceRegistry

import           Test.Dynamic.Network
import           Test.Dynamic.TxGen
import           Test.Dynamic.Util
import           Test.Dynamic.Util.NodeJoinPlan
import           Test.Dynamic.Util.NodeTopology
import           Test.Dynamic.Util.OutagesPlan

import           Test.Util.FS.Sim.MockFS (MockFS)
import qualified Test.Util.FS.Sim.MockFS as Mock
import           Test.Util.Orphans.Arbitrary ()
import           Test.Util.Orphans.IOLike ()
import           Test.Util.Orphans.NoUnexpectedThunks ()
import           Test.Util.Range

{-------------------------------------------------------------------------------
  Configuring tests
-------------------------------------------------------------------------------}

data TestConfig = TestConfig
  { numCoreNodes :: !NumCoreNodes
  , numSlots     :: !NumSlots
  , nodeJoinPlan :: !NodeJoinPlan
  , nodeTopology :: !NodeTopology
  , outagesPlan  :: !OutagesPlan
  , latencySeed  :: !(LatencyInjection SM.SMGen)
  }
  deriving (Show)

noLatencySeed :: LatencyInjection a
noLatencySeed = DoNotInjectLatencies

genLatencySeed :: Gen (LatencyInjection SM.SMGen)
genLatencySeed = (InjectLatencies . SM.mkSMGen) <$> arbitrary

shrinkLatencySeed :: LatencyInjection SM.SMGen -> [LatencyInjection SM.SMGen]
shrinkLatencySeed li =
    takeWhile (diffCtor li) [DoNotInjectLatencies, InjectTrivialLatencies]
  where
    diffCtor DoNotInjectLatencies   DoNotInjectLatencies   = False
    diffCtor InjectTrivialLatencies InjectTrivialLatencies = False
    diffCtor InjectLatencies{}      InjectLatencies{}      = False
    diffCtor _                      _                      = True

genTestConfig :: NumCoreNodes -> NumSlots -> Gen TestConfig
genTestConfig numCoreNodes numSlots = do
    nodeJoinPlan <- genNodeJoinPlan numCoreNodes numSlots
    nodeTopology <- genNodeTopology numCoreNodes
    outagesPlan <- genOutagesPlan numSlots nodeJoinPlan nodeTopology
    latencySeed <- genLatencySeed
    pure TestConfig
      { numCoreNodes
      , numSlots
      , nodeJoinPlan
      , nodeTopology
      , outagesPlan
      , latencySeed
      }

-- | Shrink and also include the original value
--
-- See 'unShrinkIdem'.
--
shrinkIdem :: (a -> [a]) -> a -> [a]
shrinkIdem f x = f x ++ [x]

-- | Discard the idempotent 'shrink'
--
-- A 'shrink' function defined as a list comprehension in which every input
-- uses 'shrinkIdem' would include the original value being 'shrink'ed. Thus
-- such comprehensions must be wrapped in this function.
unShrinkIdem :: [a] -> [a]
unShrinkIdem = init

-- | Shrink without changing the number of nodes or slots
shrinkTestConfig :: TestConfig -> [TestConfig]
shrinkTestConfig
  testConfig@TestConfig{nodeJoinPlan, nodeTopology, latencySeed} =
    unShrinkIdem $
    [ testConfig{nodeJoinPlan = p', nodeTopology = top', latencySeed = seed'}
    | p'    <- shrinkIdem shrinkNodeJoinPlan nodeJoinPlan
    , top'  <- shrinkIdem shrinkNodeTopology nodeTopology
    , seed' <- shrinkIdem shrinkLatencySeed latencySeed
    ]

-- | Shrink, including the number of nodes and slots
shrinkTestConfigFreely :: TestConfig -> [TestConfig]
shrinkTestConfigFreely TestConfig
  { numCoreNodes
  , numSlots
  , nodeJoinPlan
  , nodeTopology
  , outagesPlan
  , latencySeed
  } =
    unShrinkIdem $
    [ TestConfig
        { numCoreNodes = n'
        , numSlots = t'
        , nodeJoinPlan = p'
        , nodeTopology = top'
        , outagesPlan = op'
        , latencySeed = seed'
        }
    | n'    <- shrinkIdem shrink numCoreNodes
    , t'    <- shrinkIdem shrink numSlots
    , let adjustedP   = adjustedNodeJoinPlan n' t'
    , let adjustedTop = adjustedNodeTopology n'
    , let adjustedOp  = truncateOutagesPlan n' t' outagesPlan
    , p'    <- shrinkIdem shrinkNodeJoinPlan adjustedP
    , top'  <- shrinkIdem shrinkNodeTopology adjustedTop
    , seed' <- shrinkIdem shrinkLatencySeed latencySeed
    , op'   <- shrinkIdem shrinkOutagesPlan adjustedOp
    ]
  where
    adjustedNodeJoinPlan (NumCoreNodes n') (NumSlots t') =
        NodeJoinPlan $
        -- scale by t' / t
        Map.map (\(SlotNo i) -> SlotNo $ (i * toEnum t') `div` toEnum t) $
        -- discard discarded nodes
        Map.filterWithKey (\(CoreNodeId nid) _ -> nid < n') $
        m
      where
        NumSlots t = numSlots
        NodeJoinPlan m = nodeJoinPlan

    adjustedNodeTopology (NumCoreNodes n') =
        NodeTopology $ Map.filterWithKey (\(CoreNodeId i) _ -> i < n') m
      where
        NodeTopology m = nodeTopology

instance Arbitrary TestConfig where
  arbitrary = join $ genTestConfig <$> arbitrary <*> arbitrary
  shrink = shrinkTestConfigFreely

{-------------------------------------------------------------------------------
  Running tests
-------------------------------------------------------------------------------}

-- | Thin wrapper around 'runNodeNetwork'
--
-- Provides a 'ResourceRegistry' and 'BlockchainTime', runs in the IO sim
-- monad.
--
runTestNetwork ::
  forall blk.
     ( RunNode blk
     , TxGen blk
     , TracingConstraints blk
     )
  => (CoreNodeId -> ProtocolInfo blk)
  -> TestConfig
  -> Seed
  -> TestOutput blk
runTestNetwork pInfo
  TestConfig
    { numCoreNodes
    , numSlots
    , nodeJoinPlan
    , nodeTopology
    , outagesPlan
    , latencySeed
    }
  seed = runSimOrThrow $ do
    registry  <- unsafeNewRegistry

    -- the latest slot that is ready to start
    latestReadySlot <- uncheckedNewTVarM (SlotNo 0)
    -- a slot cannot end before a later slot is ready to start
    let waitOn s = atomically $ do
          x <- readTVar latestReadySlot
          check (s < x)
    testBtime <- newTestBlockchainTime registry numSlots waitOn

    runNodeNetwork NodeNetworkArgs
      { nnaLatencySeed         = latencySeed
      , nnaLatestReadySlot     = latestReadySlot
      , nnaMaxLatencies    = MaxLatencies
          { maxSendLatency = 10   -- io-sim "seconds"
          , maxVar1Latency = 1000   -- io-sim "seconds"
          }
      , nnaNodeJoinPlan        = nodeJoinPlan
      , nnaQuiescenceThreshold = 50000   -- io-sim "seconds"
      , nnaNodeTopology        = nodeTopology
      , nnaNumCoreNodes        = numCoreNodes
      , nnaOutagesPlan         = outagesPlan
      , nnaProtocol            = pInfo
      , nnaRegistry            = registry
      , nnaTestBtime           = testBtime
      , nnaTxSeed              = seedToChaCha seed
      }

{-------------------------------------------------------------------------------
  Test properties
-------------------------------------------------------------------------------}

-- | The properties always required
--
-- Includes:
--
-- * The competitive chains at the end of the simulation respect the expected
--   bound on fork length
-- * The nodes do not leak file handles
--
prop_general ::
  forall blk.
     ( Condense blk
     , Condense (HeaderHash blk)
     , Eq blk
     , HasHeader blk
     , RunNode blk
     )
  => SecurityParam
  -> TestConfig
  -> Maybe LeaderSchedule
  -> TestOutput blk
  -> Property
prop_general k TestConfig
  { numSlots
  , nodeJoinPlan
  , nodeTopology
  , outagesPlan
  , latencySeed
  } mbSchedule TestOutput
  { testOutputMPEEs
  , testOutputNodes
  , testOutputOnsetTips
  } =
    counterexample ("nodeChains: " <> unlines ("" : map (\x -> "  " <> condense x) (Map.toList nodeChains))) $
    counterexample ("nodeJoinPlan: " <> condense nodeJoinPlan) $
    counterexample ("nodeTopology: " <> condense nodeTopology) $
    counterexample ("outagesPlan: " <> condense outagesPlan) $
    counterexample ("latencySeed: " <> show latencySeed) $
    counterexample ("slot-node-tipBlockNo: " <> condense tipBlockNos) $
    counterexample ("mbSchedule: " <> condense mbSchedule) $
    counterexample ("growth schedule: " <> condense growthSchedule) $
    counterexample ("actual leader schedule: " <> condense actualLeaderSchedule) $
    counterexample ("consensus expected: " <> show isConsensusExpected) $
    tabulate "consensus expected" [show isConsensusExpected] $
    tabulate "shortestLength" [show (rangeK k (shortestLength nodeChains))] $
    tabulate "floor(4 * lastJoinSlot / numSlots)" [show lastJoinSlot] $
    tabulate "minimumDegreeNodeTopology" [show (minimumDegreeNodeTopology nodeTopology)] $
    tabulate "no outages" [show noOutages] $
    prop_synced_edges .&&.
    (property (not noOutages) .||.
       prop_all_common_prefix
          maxForkLength
          (Map.elems nodeChains) .&&.
       prop_all_growth .&&.
       prop_no_unexpected_message_delays
    ) .&&.
    conjoin
      [ fileHandleLeakCheck cid nodeDBs
      | (cid, nodeDBs) <- Map.toList nodeOutputDBs ]
  where
    schedule = case mbSchedule of
        Nothing    -> actualLeaderSchedule
        Just sched -> sched

    NumBlocks maxForkLength = determineForkLength k nodeJoinPlan schedule

    -- build a leader schedule which includes every node that forged unless:
    --
    -- * the node rejected its own new block (eg 'PBftExceededSignThreshold')
    --
    -- * the node forged an EBB
    --
    actualLeaderSchedule :: LeaderSchedule
    actualLeaderSchedule =
        foldl (<>) (emptyLeaderSchedule numSlots) $
        [ let NodeOutput
                { nodeOutputForges
                , nodeOutputInvalids
                } = no
          in
          LeaderSchedule $
          Map.mapMaybeWithKey
              (actuallyLead cid nodeOutputInvalids)
              nodeOutputForges
        | (cid, no) <- Map.toList testOutputNodes
        ]
      where
        actuallyLead ::
             CoreNodeId
          -> Set (Point blk)
          -> SlotNo
          -> blk
          -> Maybe [CoreNodeId]
        actuallyLead cid invalids s b = do
            let j = coreNodeIdJoinSlot nodeJoinPlan cid
            guard $ j <= s

            guard $ not $
                nodeIsEBB b || Set.member (Chain.blockPoint b) invalids

            pure [cid]

    -- Refine 'actualLeaderSchedule' to also ignore a leader if:
    --
    -- * the node just joined in this slot (unless it's the earliest slot in
    --   which any nodes joined)
    --
    growthSchedule :: LeaderSchedule
    growthSchedule =
        LeaderSchedule $ Map.mapWithKey (\s -> filter (keep s)) mlead
      where
        LeaderSchedule mlead = actualLeaderSchedule

        keep s cid =
             isFirstJoinSlot s
          || coreNodeIdJoinSlot nodeJoinPlan cid < s

        isFirstJoinSlot s =
            Just s == (snd <$> Map.lookupMin mjoin)
          where
            NodeJoinPlan mjoin = nodeJoinPlan

    nodeChains    = nodeOutputFinalChain <$> testOutputNodes
    nodeOutputDBs = nodeOutputNodeDBs    <$> testOutputNodes
    nodeForges    = nodeOutputForges     <$> testOutputNodes

    noOutages = outagesPlan == emptyOutagesPlan

    isConsensusExpected :: Bool
    isConsensusExpected =
         noOutages
      && consensusExpected k nodeJoinPlan schedule

    fileHandleLeakCheck :: CoreNodeId -> NodeDBs MockFS -> Property
    fileHandleLeakCheck cid nodeDBs = conjoin
        [ checkLeak "ImmutableDB" $ nodeDBsImm nodeDBs
        , checkLeak "VolatileDB"  $ nodeDBsVol nodeDBs
        , checkLeak "LedgerDB"    $ nodeDBsLgr nodeDBs
        ]
      where
        checkLeak dbName fs = counterexample
          ("Node " <> show cid <> "'s " <> dbName <> " is leaking file handles")
          (Mock.numOpenHandles fs === 0)

    -- in which quarter of the simulation does the last node join?
    lastJoinSlot :: Maybe Word64
    lastJoinSlot =
        fmap (\(SlotNo i, _) -> (4 * i) `div` toEnum t) $
        Map.maxView m
      where
        NumSlots t = numSlots
        NodeJoinPlan m = nodeJoinPlan

    -- check for Chain Growth violations if there are no Common Prefix
    -- violations
    --
    -- We consider all possible non-empty intervals, so the interval span
    -- @s@ varies but is always at least 1. We compute a different /speed
    -- coefficient/ @τ@ for each interval under the assumption that there are
    -- no message delays (ie @Δ = 0@). This is essentially a count of the
    -- active slots for that interval in the refined @growthSchedule@.
    --
    -- The paper <https://eprint.iacr.org/2017/573/20171115:00183> defines
    -- Common Growth as follows.
    --
    -- * Chain Growth (CG); with parameters τ ∈ (0, 1], s ∈ N. Consider the
    --   chains C1, C2 possessed by two honest parties at the onset of two
    --   slots sl1, sl2 with sl2 at least s slots ahead of sl1. Then it holds
    --   that len(C2) − len(C1) ≥ τs. We call τ the speed coefficient.
    prop_all_growth =
        isConsensusExpected `implies`
            conjoin
                [ prop_growth (s1, max1) (s2, min2)
                | ((s1, _, max1), (s2, min2, _)) <- orderedPairs extrema
                ]
      where
        -- all pairs @(x, y)@ where @x@ precedes @y@ in the given list
        orderedPairs :: [a] -> [(a, a)]
        orderedPairs = \case
            []   -> []
            x:ys -> foldr ((:) . (,) x) (orderedPairs ys) ys

        prop_growth :: (SlotNo, BlockNo) -> (SlotNo, BlockNo) -> Property
        prop_growth (s1, b1) (s2, b2) =
            counterexample (condense (s1, s2, b1, b2, numActiveSlots)) $
            nonNegativeGrowth .&&.
            sufficientGrowth
          where
            nonNegativeGrowth =
                counterexample "negative chain growth" $
                    property (b2 >= b1)

            sufficientGrowth =
                counterexample "insufficient chain growth" $
                    property (d >= toEnum numActiveSlots)

            BlockNo d = b2 - b1
            numActiveSlots =
                Map.size $
                flip Map.filterWithKey (getLeaderSchedule growthSchedule) $
                \slot ls -> s1 <= slot && slot < s2 && (not . null) ls

        -- @(s, min, max)@ the minimum and maximum block number of the tip of a
        -- chain at the onset of slot @s@.
        extrema :: [(SlotNo, BlockNo, BlockNo)]
        extrema =
            [ case map snd bnos' of
                  [] -> (slot, 0, 0)
                  o  -> (slot, minimum o, maximum o)
            | (slot, bnos) <- tipBlockNos
            , let bnos' = filter (joinedBefore slot . fst) bnos
            ]

    joinedBefore slot nid = nodeIdJoinSlot nodeJoinPlan nid < slot

    -- swizzled 'testOutputOnsetTips'
    tipBlockNos :: [(SlotNo, [(NodeId, BlockNo)])]
    tipBlockNos =
        Map.toAscList $
        fmap Map.toAscList $
        fmap (Map.mapKeysMonotonic fromCoreNodeId . fmap fst) $
        testOutputOnsetTips

    -- In the paper <https://eprint.iacr.org/2017/573/20171115:00183>, a
    -- /message/ carries a chain from one party to another. When a party forges
    -- a block, it \"diffuses\" the chain with that block as its head by
    -- sending a message to each other party (actually, to itself too, but
    -- that's ultimately redundant). The adversary is able to delay each
    -- message differently, so some parties may receive it before others do.
    -- Once a party receives a message, the party can consider that chain for
    -- selection.
    --
    -- In the implementation, on the other hand, our messages are varied and
    -- much more granular than a whole chain. We therefore observe a delay
    -- analogous to the paper's /message/ /delay/ by comparing the slot in
    -- which a block is added to each node's ChainDB against the slot in which
    -- that block was forged.
    --
    -- Since our mock network currently introduces only negligible latency
    -- compared to the slot duration, we generally expect all messages to have
    -- no delay: they should arrive to all nodes during the same slot in which
    -- they were forged. However, some delays are expected, due to nodes
    -- joining late and also due to the practicality of the ChainSync and
    -- BlockFetch policies, which try to avoid /unnecessary/ header/block
    -- fetches. See the relevant comments below.
    --
    -- NOTE: This current property does not check for interminable message
    -- delay: i.e. for blocks that were never added to some ChainDBs. It only
    -- checks the slot difference once a message does arrive. This seems
    -- acceptable: if there are no Common Prefix or Chain Growth violations,
    -- then each message must have either arrived or ultimately been
    -- irrelevant.
    --
    prop_no_unexpected_message_delays :: HasCallStack => Property
    prop_no_unexpected_message_delays =
        conjoin $
        [ case p of
              GenesisPoint            -> error "impossible"
              BlockPoint sendSlot hsh ->
                  prop1 cid recvSlot sendSlot hsh bno
        | (cid, m)          <- Map.toList adds
        , (recvSlot, pbnos) <- Map.toList m
        , (p, bno)          <- Set.toList pbnos
        ]
      where
        -- INVARIANT: these AddBlock events are *not* for EBBs
        adds = nodeOutputAdds <$> testOutputNodes

        prop1 cid recvSlot sendSlot hsh bno =
            counterexample msg $
            delayOK || noDelay
          where
            msg =
                "Unexpected message delay " <>
                "(" <> "recipient: " <> condense (fromCoreNodeId cid) <>
                "," <> "expected receive slot: "
                    <> condense firstPossibleReception <>
                "," <> "actual receive slot: " <> condense recvSlot <>
                "," <> "blockHash: " <> show hsh <>
                "," <> "blockNo: " <> condense (unBlockNo bno) <>
                ")"

            -- a node cannot receive a block until both exist
            firstPossibleReception =
                coreNodeIdJoinSlot nodeJoinPlan cid `max` sendSlot

            noDelay = recvSlot == firstPossibleReception

            delayOK = delayOK1 || delayOK2

            -- When a node leads in the same slot in which it joins the
            -- network, it immediately forges a single block on top of Genesis;
            -- this block then prevents it from fetching the network's current
            -- chain if that also consists of just one block.
            --
            -- NOTE This predicate is more general than that specific scenario,
            -- but we don't anticipate it wholly masking any interesting cases.
            delayOK1 = 1 == bno

            -- When a slot has multiple leaders, each node chooses one of the
            -- mutually-exclusive forged blocks and won't fetch any of the
            -- others until it's later compelled to switch to a chain
            -- containing one of them
            --
            -- TODO This predicate is more general than that specific scenario,
            -- and should be tightened accordingly. We currently anticipate
            -- that Issues #229 and #230 will handle that.
            delayOK2 = case Map.lookup sendSlot sched of
                Just (_:_:_) -> True
                _            -> False
              where
                LeaderSchedule sched = actualLeaderSchedule

    -- If a directed edge runs to quiesence in a slot, the resulting chains
    -- must satisfy an invariant:
    --
    --   * Either the block number of the server is less than or equal to the
    --     client's and the immutable prefix of the server 'precede's that of
    --     the client
    --
    --   * or the 'immutablePrefix' of the client does not 'precede' the
    --     server's chain.
    --
    prop_synced_edges :: HasCallStack => Property
    prop_synced_edges =
        conjoin $ map slotProp (finalTips : onsetTips)
      where
        -- tips at the onset of each slot
        onsetTips :: [(SlotNo, Map CoreNodeId (Point blk))]
        onsetTips = Map.toList (fmap snd <$> testOutputOnsetTips)

        -- tips at the onset of the hypothetical slot after the run
        finalTips :: (SlotNo, Map CoreNodeId (Point blk))
        finalTips =
          ( SlotNo (fromIntegral t)
          , Chain.headPoint <$> nodeChains
          )
          where
            NumSlots t = numSlots

        -- we check a slot's chains at onset against the edges that were still
        -- up when the __previous__ slot quiesced
        slotProp (SlotNo 0, _    ) = property True
        slotProp (s,        mTips) = slotProp2 (pred s) mTips

        slotProp2 s mTips =
          conjoin $ map (edgeProp s mTips) (upEdges s)

        upEdges :: SlotNo -> [OutageEdge]
        upEdges s = Set.toList $
            foldMap (bothDirections s) (edgesNodeTopology nodeTopology)
          `Set.difference`
            fromMaybe Set.empty (Map.lookup s testOutputMPEEs)
          `Set.difference`
            plannedSlotOutageEdges s outagesPlan

        bothDirections :: SlotNo -> OutageEdge -> Set OutageEdge
        bothDirections s (n1, n2)
          | s < max (joinSlot n1) (joinSlot n2) = Set.empty
          | otherwise                           =
            Set.insert (n1, n2) $ Set.singleton (n2, n1)
          where
            joinSlot = coreNodeIdJoinSlot nodeJoinPlan

        -- the @(client, server)@ ordering is determined by
        -- 'Test.Dynamic.Network.directedEdge'
        edgeProp s mTips (client, server) =
            case (,) <$> lu client <*> lu server of
              Nothing -> error $ "could not find final tips: "
                <> show (s, client, server)
              Just x  -> edgeProp2 s `uncurry` x
          where
            lu n = (,) n <$> Map.lookup n mTips

        -- the expected invariant
        --
        -- see the comments on the antecedents and conjunctions within this
        -- definition
        edgeProp2 ::
              SlotNo
           -> (CoreNodeId, Point blk)
              -- ^ client tip after the slot quiesced
           -> (CoreNodeId, Point blk)
              -- ^ server too
           -> Property
        edgeProp2 s (n1, p1) (n2, p2) =
            counterexample msg $
            implies ante $
            counterexample "conj1" conj1 .&&.
            counterexample "conj2" conj2
          where
            c1 = lookupPoint p1
            c2 = lookupPoint p2
            imm1 = immutablePrefix k snd c1
            imm2 = immutablePrefix k snd c2

            getBNo :: Chain (HeaderHash blk, BlockNo) -> BlockNo
            getBNo = chainBlockNo snd

            notDeeper ::
                 Chain (HeaderHash blk, BlockNo)
              -> Chain (HeaderHash blk, BlockNo)
              -> Bool
            notDeeper = on (<=) getBNo

            pre ::
                 Chain (HeaderHash blk, BlockNo)
              -> Chain (HeaderHash blk, BlockNo)
              -> Bool
            pre = precedes ((==) `on` fst) snd

            -- if false, then -- at least by the end of the slot -- the
            -- security parameter k would prevent the client from switching to
            -- the server's chain
            ante :: Bool
            ante = imm1 `pre` c2

            -- the client's chain must be at least as long as the server's
            conj1 :: Bool
            conj1 = c2 `notDeeper` c1

            -- the client's immutable tip must be an extension of the server's
            conj2 :: Bool
            conj2 = imm2 `pre` imm1

            msg :: String
            msg =
              unlines $
              [ "An edge failed to fully sync!"
              , "slot: " <> show s
              , "client: " <> condense (fromCoreNodeId n1, p1, getBNo c1)
              , "server: " <> condense (fromCoreNodeId n2, p2, getBNo c2)
              , "ante: " <> show ante
              , "conj1: " <> show conj1
              , "conj2: " <> show conj2
              ]

        lookupPoint :: Point blk -> Chain (HeaderHash blk, BlockNo)
        lookupPoint = lookupChainHash . pointHash

        -- use all nodes' 'nodeOutputForges's to map a hash to its full chain
        lookupChainHash :: ChainHash blk -> Chain (HeaderHash blk, BlockNo)
        lookupChainHash = \case
            GenesisHash -> Chain.Genesis
            BlockHash h ->
              case Map.lookup h memoTable of
                Nothing -> error $ "unknown block hash: " <> show h
                Just c  -> c
          where
            memoTable = foldl snoc Map.empty (foldMap Map.elems nodeForges)
            snoc acc blk = Map.insert h (prev Chain.:> (h, bno)) acc
              where
                h    = Chain.blockHash blk
                bno  = Chain.blockNo blk
                prev = lookupChainHash (Chain.blockPrevHash blk)

{-------------------------------------------------------------------------------
  Property utilies
-------------------------------------------------------------------------------}

-- | Require a 'Property' conditionally
--
-- QuickCheck's @==>@ is similar, but goes further by 'discard'ing the test if
-- the antecedent is false.
--
implies :: Bool -> Property -> Property
implies p1 p2 = not p1 .||. p2

-- | Test whether P is either a prefix of or else equal to Q
--
precedes ::
     (a -> a -> Bool)
     -- ^ equal
  -> (a -> BlockNo)
  -> Chain a
  -> Chain a
  -> Bool
precedes eq f c1 c2 =
    case skipEBBs f c1 of
      Chain.Genesis -> True
      _ Chain.:> a1 -> go a1 (skipEBBs f c2)
  where
    go a1 = \case
      Chain.Genesis     -> False
      prev2 Chain.:> a2 ->
        case (compare `on` f) a1 a2 of
          LT -> go a1 prev2
          EQ -> if eq a1 a2 then True else go a1 prev2
          GT -> False

-- | Filter out blocks with the same block number as their predecessor
--
-- Notably, the result has no EBBs.
--
skipEBBs :: (a -> BlockNo) -> Chain a -> Chain a
skipEBBs f = \case
    Chain.Genesis -> Chain.Genesis
    c Chain.:> a  ->
      (if f a == chainBlockNo f c then id else (Chain.:> a)) $
      skipEBBs f c

chainBlockNo :: (a -> BlockNo) -> Chain a -> BlockNo
chainBlockNo f = \case
  Chain.Genesis -> 0
  _ Chain.:> a  -> f a

-- | The shortest prefix of C whose block number plus the security parameter k
-- equals C's block number
--
-- Notably, the prefix never ends in an EBB.
--
immutablePrefix :: SecurityParam -> (a -> BlockNo) -> Chain a -> Chain a
immutablePrefix k f = \c -> go (maxRollbacks k) (chainBlockNo f c) c
  where
    go toRollback bno c = case c of
      Chain.Genesis        -> Chain.Genesis
      c' Chain.:> b
        | bno == f b      -> go toRollback (f b) c'
        | 0 == toRollback -> c
        | otherwise       -> go (toRollback - 1) (f b) c'


{-
commonPrefix ::
     (a -> a -> Bool)
     -- ^ equal
  -> (a -> BlockNo)
  -> Chain a
  -> Chain a
  -> Chain a
commonPrefix eq f = go
  where
    go Chain.Genesis       _                   = Chain.Genesis
    go _                   Chain.Genesis       = Chain.Genesis
    go c1@(p1 Chain.:> a1) c2@(p2 Chain.:> a2) =
      case (compare `on` f) a1 a2 of
        LT -> go c1 p2
        EQ -> if eq a1 a2 then c1 else go p1 p2
        GT -> go p1 c2
-}
