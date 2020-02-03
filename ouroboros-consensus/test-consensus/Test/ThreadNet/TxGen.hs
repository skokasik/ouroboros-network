{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Transaction generator for testing
module Test.ThreadNet.TxGen
  ( TxGen (..)
  ) where

import           Control.Monad (replicateM)
import           Crypto.Number.Generate (generateBetween)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           GHC.Stack (HasCallStack)

import           Ouroboros.Network.Block (SlotNo (..))

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Byron
import           Ouroboros.Consensus.Ledger.Mock hiding (utxo)
import           Ouroboros.Consensus.Node.ProtocolInfo (NumCoreNodes (..))
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Random

{-------------------------------------------------------------------------------
  TxGen class
-------------------------------------------------------------------------------}

class TxGen blk where
  -- | Generate a transaction, valid or invalid, that can be submitted to a
  -- node's Mempool.
  testGenTx :: MonadRandom m
            => NumCoreNodes
            -> SlotNo
            -> NodeConfig (BlockProtocol blk)
            -> LedgerState blk
            -> m (GenTx blk)

  -- | Generate a number of transactions, valid or invalid, that can be
  -- submitted to a node's Mempool.
  --
  -- This function (not 'testGenTx') will be called to generate transactions
  -- in consensus tests.
  testGenTxs :: MonadRandom m
             => NumCoreNodes
             -> SlotNo
             -> NodeConfig (BlockProtocol blk)
             -> LedgerState blk
             -> m [GenTx blk]
  testGenTxs  numCoreNodes curSlotNo cfg ledger = do
    -- Currently 0 to 1 txs
    n <- generateBetween 0 20
    replicateM (fromIntegral n) $ testGenTx numCoreNodes curSlotNo cfg ledger

  {-# MINIMAL testGenTx #-}

{-------------------------------------------------------------------------------
  TxGen SimpleBlock
-------------------------------------------------------------------------------}

instance TxGen (SimpleBlock SimpleMockCrypto ext) where
  testGenTx numCoreNodes curSlotNo _cfg ledgerState =
      mkSimpleGenTx <$> genSimpleTx curSlotNo addrs utxo
    where
      addrs :: [Addr]
      addrs = Map.keys $ mkAddrDist numCoreNodes

      utxo :: Utxo
      utxo = mockUtxo $ simpleLedgerState ledgerState

genSimpleTx :: forall m. MonadRandom m => SlotNo -> [Addr] -> Utxo -> m Tx
genSimpleTx curSlotNo addrs u = do
    let senders = Set.toList . Set.fromList . map fst . Map.elems $ u -- people with funds
    sender    <- genElt senders
    recipient <- genElt $ filter (/= sender) addrs
    let assets  = filter (\(_, (a, _)) -> a == sender) $ Map.toList u
        fortune = sum [c | (_, (_, c)) <- assets]
        ins     = Set.fromList $ map fst assets
    amount <- fromIntegral <$> generateBetween 1 (fromIntegral fortune)
    let outRecipient = (recipient, amount)
        outs         = if amount == fortune
            then [outRecipient]
            else [outRecipient, (sender, fortune - amount)]
    expiry <- (mkExpiry . SlotNo . fromIntegral) <$> generateBetween 2 10
    return $ Tx expiry ins outs
  where
    mkExpiry :: SlotNo -> Expiry
    mkExpiry delta = ExpireAtOnsetOf $ curSlotNo + delta

    genElt :: HasCallStack => [a] -> m a
    genElt xs = do
        m <- generateElement xs
        case m of
            Nothing -> error "expected non-empty list"
            Just x  -> return x

{-------------------------------------------------------------------------------
  TxGen ByronBlock
-------------------------------------------------------------------------------}

instance TxGen ByronBlock where
  testGenTx = error "TODO #855 testGenTx"
  -- 'testGenTxs' is used by the tests, not 'testGenTx'.
  testGenTxs _ _ _ _ = return []
