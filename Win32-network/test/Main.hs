{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module Main (main) where

import           Test.Tasty

import qualified Test.NamedPipes
import qualified Test.Event

main :: IO ()
main = defaultMain $ testGroup "Win32"
  [ Test.NamedPipes.tests
  , Test.Event.tests
  ]
   
