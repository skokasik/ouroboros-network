{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE BangPatterns        #-}

module Ouroboros.Network.TxSubmission.Inbound (
    txSubmissionInbound,
    TxSubmissionMempoolWriter(..),
    TraceTxSubmissionInbound(..),
    TxSubmissionProtocolError(..),
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import           Data.Word (Word16)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Data.Sequence.Strict as Seq
import           Data.Sequence.Strict (StrictSeq)
import           Data.Foldable (foldl')

import           Control.Monad (unless)
import           Control.Monad.Class.MonadSTM hiding (modifyTVar, readTVar)
import           Control.Monad.Class.MonadSTM.Strict (StrictTVar, modifyTVar,
                     readTVar)
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime (MonadTime (..), Time (..),
                     addTime)
import           Control.Exception (assert)
import           Control.Tracer (Tracer)

import           Network.TypedProtocol.Pipelined (N, Nat(..))

import           Ouroboros.Network.Protocol.TxSubmission.Server
import qualified Ouroboros.Network.RecentTxIds as RecentTxIds (insertTxIds,
                     intersection)
import           Ouroboros.Network.RecentTxIds (RecentTxIds)



-- | The consensus layer functionality that the inbound side of the tx
-- submission logic requires.
--
-- This is provided to the tx submission logic by the consensus layer.
--
data TxSubmissionMempoolWriter txid tx idx m =
     TxSubmissionMempoolWriter {

       -- | Compute the transaction id from a transaction.
       --
       -- This is used in the protocol handler to verify a full transaction
       -- matches a previously given transaction id.
       --
       txId :: tx -> txid,

       -- | Supply a batch of transactions to the mempool. They are either
       -- accepted or rejected individually, but in the order supplied.
       --
       -- The 'txid's of all transactions that were added successfully are
       -- returned.
       mempoolAddTxs :: [tx] -> m [txid]
    }

data TraceTxSubmissionInbound txid tx = TraceTxSubmissionInbound --TODO
  deriving Show

data TxSubmissionProtocolError =
       ProtocolErrorTxNotRequested

  deriving Show

instance Exception TxSubmissionProtocolError where
  displayException ProtocolErrorTxNotRequested =
      "The peer replied with a transaction we did not ask for."


-- | Information maintained internally in the 'txSubmissionInbound' server
-- implementation.
--
data ServerState txid tx = ServerState {
       -- | The number of transaction identifiers that we have requested but
       -- which have not yet been replied to. We need to track this it keep
       -- our requests within the limit on the number of unacknowledged txids.
       --
       requestedTxIdsInFlight :: Word16,

       -- | Those transactions (by their identifier) that the client has told
       -- us about, and which we have not yet acknowledged. This is kept in
       -- the order in which the client gave them to us. This is the same order
       -- in which we submit them to the mempool (or for this example, the final
       -- result order). It is also the order we acknowledge in.
       unacknowledgedTxIds :: StrictSeq txid,

       -- | Those transactions (by their identifier) that we can request. These
       -- are a subset of the 'unacknowledgedTxIds' that we have not yet
       -- requested. This is not ordered to illustrate the fact that we can
       -- request txs out of order. We also remember the size.
       availableTxids      :: Map txid TxSizeInBytes,

       -- | Transactions we have successfully downloaded but have not yet added
       -- to the mempool or acknowledged. This needed because we can request
       -- transactions out of order but must use the original order when adding
       -- to the mempool or acknowledging transactions.
       --
       -- However, it's worth noting that, in a few situations, some of the
       -- transaction IDs in this 'Map' may be mapped to 'Nothing':
       --
       -- * Transaction IDs mapped to 'Nothing' can represent transaction IDs
       --   that were requested, but not received. This can occur because the
       --   client will not necessarily send all of the transactions that we
       --   asked for, but we still need to acknowledge those transactions.
       --
       --   For example, if we request a transaction that no longer exists in
       --   the client's mempool, the client will just exclude it from the
       --   response. However, we still need to acknowledge it (i.e. remove it
       --   from the 'unacknowledgedTxIds') in order to note that we're no
       --   longer awaiting receipt of that transaction.
       --
       -- * Transaction IDs mapped to 'Nothing' can represent transactions
       --   that were not requested from the client because they've already
       --   been recently processed and added to the mempool by the
       --   transaction submission server.
       --
       --   For example, if we request some transaction IDs and notice that
       --   some subset of them have already been recently added to the
       --   mempool by the tx submission server, we wouldn't want to bother
       --   asking for those specific transactions. Therefore, we would just
       --   insert those transaction IDs mapped to 'Nothing' to the
       --   'bufferedTxs' such that those transactions are acknowledged, but
       --   never actually requested.
       --
       bufferedTxs         :: Map txid (Maybe tx),

       -- | The number of transactions we can acknowledge on our next request
       -- for more transactions. The number here have already been removed from
       -- 'unacknowledgedTxIds'.
       --
       numTxsToAcknowledge :: Word16
     }
  deriving Show

initialServerState :: ServerState txid tx
initialServerState = ServerState 0 Seq.empty Map.empty Map.empty 0


txSubmissionInbound
  :: forall txid tx idx m.
     (Ord txid, Ord idx, MonadSTM m, MonadThrow m, MonadTime m)
  => Tracer m (TraceTxSubmissionInbound txid tx)
  -> Word16
  -- ^ Maximum number of unacknowledged txids allowed
  -> Time
  -- ^ The threshold at which transaction IDs in the 'RecentTxIds' should
  -- expire.
  -> StrictTVar m (RecentTxIds txid)
  -- ^ A collection of transaction IDs that we've most recently added to the
  -- mempool from instances of the transaction submission server.
  -> TxSubmissionMempoolWriter txid tx idx m
  -> TxSubmissionServerPipelined txid tx m ()
txSubmissionInbound _tracer maxUnacked txIdExpThresh recentTxIdsVar mpWriter =
    TxSubmissionServerPipelined (serverIdle Zero initialServerState)
  where
    --TODO: replace these fixed limits by policies based on TxSizeInBytes
    -- and delta-Q and the bandwidth/delay product.
    -- These numbers are for demo purposes only, the throughput will be low.
    maxTxIdsToRequest = 3 :: Word16
    maxTxToRequest    = 2 :: Word16

    TxSubmissionMempoolWriter
      { txId
      , mempoolAddTxs
      } = mpWriter

    serverIdle :: forall (n :: N).
                  Nat n
               -> ServerState txid tx
               -> m (ServerStIdle n txid tx m ())
    serverIdle Zero st
        -- There are no replies in flight, but we do know some more txs we can
        -- ask for, so lets ask for them and more txids.
      | canRequestMoreTxs st
      = do
        serverReqTxsRes <-serverReqTxs Zero st
        case serverReqTxsRes of
          Left st'  -> serverIdle Zero st'
          Right res -> pure res

        -- There's no replies in flight, and we have no more txs we can ask for
        -- so the only remaining thing to do is to ask for more txids. Since
        -- this is the only thing to do now, we make this a blocking call.
      | otherwise
      , let numTxIdsToRequest = maxTxIdsToRequest `min` maxUnacked
      = assert (requestedTxIdsInFlight st == 0
             && Seq.null (unacknowledgedTxIds st)
             && Map.null (availableTxids st)
             && Map.null (bufferedTxs st)) $
        pure $
        SendMsgRequestTxIdsBlocking
          (numTxsToAcknowledge st)
          numTxIdsToRequest
          ()                -- Our result if the client terminates the protocol
          ( handleReply Zero st {
              numTxsToAcknowledge    = 0,
              requestedTxIdsInFlight = numTxIdsToRequest
            }
          . CollectTxIds numTxIdsToRequest
          . NonEmpty.toList)

    serverIdle (Succ n) st
        -- We have replies in flight and we should eagerly collect them if
        -- available, but there are transactions to request too so we should
        -- not block waiting for replies.
        --
        -- Having requested more transactions, we opportunistically ask for
        -- more txids in a non-blocking way. This is how we pipeline asking for
        -- both txs and txids.
        --
        -- It's important not to pipeline more requests for txids when we have
        -- no txs to ask for, since (with no other guard) this will put us into
        -- a busy-polling loop.
        --
      | canRequestMoreTxs st
      = do
        serverReqTxsRes <- serverReqTxs (Succ n) st
        case serverReqTxsRes of
          Left st' ->
            pure $ CollectPipelined
              Nothing
              (handleReply n st')
          Right res ->
            pure $ CollectPipelined
              (Just res)
              (handleReply n st)

        -- In this case there is nothing else to do so we block until we
        -- collect a reply.
      | otherwise
      = pure $ CollectPipelined
          Nothing
          (handleReply n st)

    canRequestMoreTxs :: ServerState k tx -> Bool
    canRequestMoreTxs st =
        not (Map.null (availableTxids st))

    handleReply :: forall (n :: N).
                   Nat n
                -> ServerState txid tx
                -> Collect txid tx
                -> m (ServerStIdle n txid tx m ())
    handleReply n st (CollectTxIds reqNo txids) =
      -- Upon receiving a batch of new txids we extend our available set,
      -- and extended the unacknowledged sequence.
      serverIdle n st {
        requestedTxIdsInFlight = requestedTxIdsInFlight st - reqNo,
        unacknowledgedTxIds    = unacknowledgedTxIds st
                              <> Seq.fromList (map fst txids),
        availableTxids         = availableTxids st
                              <> Map.fromList txids
      }

    handleReply n st (CollectTxs txids txs) = do

      -- To start with we have to verify that the txs they have sent us do
      -- correspond to the txs we asked for. This is slightly complicated by
      -- the fact that in general we get a subset of the txs that we asked for.
      -- We should never get a tx we did not ask for. We take a strict approch
      -- to this and check it.
      --
      let txsMap :: Map txid tx
          txsMap = Map.fromList [ (txId tx, tx) | tx <- txs ]

          txidsReceived  = Map.keysSet txsMap
          txidsRequested = Set.fromList txids

      unless (txidsReceived `Set.isSubsetOf` txidsRequested) $
        throwM ProtocolErrorTxNotRequested

          -- We can match up all the txids we requested, with those we received.
      let txIdsRequestedWithTxsReceived :: Map txid (Maybe tx)
          txIdsRequestedWithTxsReceived =
              Map.map Just txsMap
           <> Map.fromSet (const Nothing) txidsRequested

          -- We still have to acknowledge the txids we were given. This
          -- combined with the fact that we request txs out of order means our
          -- bufferedTxs has to track all the txids we asked for, even though
          -- not all have replies.
          bufferedTxs' = bufferedTxs st <> txIdsRequestedWithTxsReceived

          -- We have to update the unacknowledgedTxIds here eagerly and not
          -- delay it to serverReqTxs, otherwise we could end up blocking in
          -- serverIdle on more pipelined results rather than being able to
          -- move on.

          -- Check if having received more txs we can now confirm any (in
          -- strict order in the unacknowledgedTxIds sequence).
          (acknowledgedTxIds, unacknowledgedTxIds') =
            Seq.spanl (`Map.member` bufferedTxs') (unacknowledgedTxIds st)

          -- If so we can submit the acknowledged txs to our local mempool
          txsReady = foldr (\txid r -> maybe r (:r) (bufferedTxs' Map.! txid))
                           [] acknowledgedTxIds

          -- And remove acknowledged txs from our buffer
          bufferedTxs'' = foldl' (flip Map.delete)
                                 bufferedTxs' acknowledgedTxIds

      addedTxIds <- mempoolAddTxs txsReady

      -- Insert the transactions that were added to the mempool into the
      -- 'RecentTxIds'.
      let Time expThreshDiffTime = txIdExpThresh
      currTime <- getMonotonicTime
      atomically $ modifyTVar
        recentTxIdsVar
        (RecentTxIds.insertTxIds addedTxIds (expThreshDiffTime `addTime` currTime))

      serverIdle n st {
        bufferedTxs         = bufferedTxs'',
        unacknowledgedTxIds = unacknowledgedTxIds',
        numTxsToAcknowledge = numTxsToAcknowledge st
                            + fromIntegral (Seq.length acknowledgedTxIds)
      }


    -- Attempt to request transactions in a pipelined fashion.
    --
    -- Before requesting the corresponding transactions for transaction IDs
    -- that we've received from the client, we first check the 'RecentTxIds'
    -- data structure in order to determine whether we've already recently
    -- requested and added those transactions to the mempool. This enables us
    -- to only request transactions that we haven't encountered recently.
    --
    -- For those transaction IDs that already exist within 'RecentTxIds', we
    -- pre-emptively acknowledge them and exclude them from our request for
    -- transactions.
    --
    -- If this function returns 'Right', this indicates that we were able to
    -- make a pipelined request for transactions.
    --
    -- If this function returns 'Left', this indicates that there were no
    -- remaining transactions that could be requested after acknowledging
    -- transaction IDs that already exist within 'RecentTxIds'. This can occur
    -- in the event that all of the 'unacknowledgedTxIds' already exist within
    -- the 'RecentTxIds' structure. In this case, we would pre-emptively
    -- acknowledge all of the 'unacknowledgedTxIds' and no longer have any
    -- transactions to request. Therefore, we just return the updated
    -- 'ServerState' and the caller should decide how to proceed based on this
    -- result.
    --
    serverReqTxs :: forall (n :: N).
                    Nat n
                 -> ServerState txid tx
                 -> m ( Either
                          (ServerState txid tx)
                          -- The updated server state returned in the event
                          -- that we were unable to request transactions after
                          -- acknowledging those that already exist in the
                          -- mempool.
                          (ServerStIdle n txid tx m ())
                          -- A pipelined request for transactions.
                      )
    serverReqTxs n st = do
        -- We can pick txs out of order if we want to, and we can decide not
        -- to pick some txs at all (if for example we have seen them already).
        -- We don't have to pick them all in one go, we can stick to a limit
        -- like maxTxToRequest.

        recentTxIds <- atomically $ readTVar recentTxIdsVar

            -- Potential transactions to request now and ones to keep to
            -- consider later.
        let (potentialTxsToRequest, availableTxids') =
              Map.splitAt (fromIntegral maxTxToRequest) (availableTxids st)

            -- Transactions to not request at all because we've already
            -- recently added them to the mempool.
            txsToIgnore = Set.fromList $
              RecentTxIds.intersection (Map.keys potentialTxsToRequest) recentTxIds

            -- Transactions to request now.
            txsToRequest = Map.withoutKeys potentialTxsToRequest txsToIgnore

            -- We still have to acknowledge the txids we were given. For txs
            -- we do ask for, we'll deal with acknowledgement after we have
            -- receive the reply, but for ones we now decide do not to ask for
            -- we have to arrange for their acknowledgement now.
            --
            -- So we extended bufferedTxs with those txs we'll not be requesting
            -- (so of course they have no corresponding reply).
            --
            bufferedTxs' = bufferedTxs st
                        <> Map.fromSet (const Nothing) txsToIgnore

            -- Check if having decided not to request more txs we can now confirm
            -- any txids (in strict order in the unacknowledgedTxIds sequence).
            -- This is used in the 'numTxsToAcknowledge' below which will then
            -- be used next time we SendMsgRequestTxIds.
            --
            (acknowledgedTxIds, unacknowledgedTxIds') =
              Seq.spanl (`Map.member` bufferedTxs') (unacknowledgedTxIds st)

            -- If so we can remove acknowledged txs from our buffer
            --
            bufferedTxs'' = foldl' (flip Map.delete)
                                   bufferedTxs' acknowledgedTxIds

            -- The next 'ServerState'
            st' = st {
                availableTxids      = availableTxids',
                bufferedTxs         = bufferedTxs'',
                unacknowledgedTxIds = unacknowledgedTxIds',
                numTxsToAcknowledge = numTxsToAcknowledge st
                                    + fromIntegral (Seq.length acknowledgedTxIds)
              }

        if (not $ null unacknowledgedTxIds')
          then
            pure $ Right $ SendMsgRequestTxsPipelined
              (Map.keys txsToRequest)
              (serverReqTxIds (Succ n) st')
          else
            -- There are no transactions that can be requested.
            -- Return the updated 'ServerState' and allow the caller to decide
            -- how to proceed.
            assert (null txsToRequest) $
            pure $ Left st'


    serverReqTxIds :: forall (n :: N).
                      Nat n
                   -> ServerState txid tx
                   -> m (ServerStIdle n txid tx m ())
    serverReqTxIds n st
      | numTxIdsToRequest > 0
      = pure $ SendMsgRequestTxIdsPipelined
          (numTxsToAcknowledge st)
          numTxIdsToRequest
          (serverIdle (Succ n) st {
                requestedTxIdsInFlight = requestedTxIdsInFlight st
                                       + numTxIdsToRequest,
                numTxsToAcknowledge    = 0
              })

      | otherwise
      = serverIdle n st
      where
        -- This definition is justified by the fact that the
        -- 'numTxsToAcknowledge' are not included in the 'unacknowledgedTxIds'.
        numTxIdsToRequest =
                (maxUnacked
                  - fromIntegral (Seq.length (unacknowledgedTxIds st))
                  - requestedTxIdsInFlight st)
          `min` maxTxIdsToRequest
