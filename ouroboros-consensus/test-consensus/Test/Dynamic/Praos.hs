{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS -fno-warn-unused-binds #-}
{-# OPTIONS -fno-warn-orphans #-}

module Test.Dynamic.Praos (
    tests
  ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Test.QuickCheck

import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Ouroboros.Network.Block
import           Ouroboros.Network.Chain
import qualified Ouroboros.Network.Chain as Chain
import           Ouroboros.Network.Node

import           Ouroboros.Consensus.Demo
import           Ouroboros.Consensus.Node (NumSlots (..))
import           Ouroboros.Consensus.Protocol.Praos
import           Ouroboros.Consensus.Util.Chain (dropLastBlocks, lastSlot)
import           Ouroboros.Consensus.Util.Condense
import           Ouroboros.Consensus.Util.Random

import           Test.Dynamic.General
import           Test.Dynamic.Util

tests :: TestTree
tests = testGroup "Dynamic chain generation" [
      testProperty "simple Praos convergence" $
        prop_simple_praos_convergence
          (NumSlots (fromIntegral numSlots))
          (NumCoreNodes 3)
          params
    ]
  where
    params@PraosParams{..} = defaultDemoPraosParams
    numSlots  = praosK * praosSlotsPerEpoch * numEpochs
    numEpochs = 3

prop_simple_praos_convergence :: NumSlots
                              -> NumCoreNodes
                              -> PraosParams
                              -> Seed
                              -> Property
prop_simple_praos_convergence numSlots numCoreNodes params =
    prop_simple_protocol_convergence
      (protocolInfo (DemoPraos params) numCoreNodes)
      isValid
      numSlots
      numCoreNodes
  where
    PraosParams{..} = params

    isValid :: [NodeId]
            -> [(VTime, Map NodeId (Chain (Block DemoPraos)))]
            -> Property
    isValid nodeIds trace = counterexample (show trace) $
      case trace of
        [(_, final)] ->   collect (shortestLength final)
                     $    Map.keys final == nodeIds
                     .&&. prop_all_common_prefix praosK (Map.elems final)
        _otherwise   -> property False

prop_all_common_prefix :: (HasHeader b, Condense b, Eq b)
                       => Word -> [Chain b] -> Property
prop_all_common_prefix _ []     = property True
prop_all_common_prefix l (c:cs) = conjoin [prop_common_prefix l c d | d <- cs]

prop_common_prefix :: forall b. (HasHeader b, Condense b, Eq b)
                   => Word -> Chain b -> Chain b -> Property
prop_common_prefix l x y = go x y .&&. go y x
  where
    go c d =
        let c' = dropLastBlocks (fromIntegral l) c
            e  = "after dropping "
                 <> show l
                 <> " blocks from "
                 <> showChain c
                 <> ", the resulting "
                 <> showChain c'
                 <> " is not a prefix of "
                 <> showChain d
        in  counterexample "" $ c' `isPrefixOf` d

    showChain :: Chain b -> String
    showChain c = condense c
                  <> "(length "
                  <> show (Chain.length c)
                  <> case lastSlot c of
                        Nothing -> ")"
                        Just s  ->    ", last slot "
                                   <> show (getSlot s)
                                   <> ")"