{-# LANGUAGE OverloadedStrings #-}

module Data.Succinct.BalancedParens.FindCloseNSpec where

import Data.Succinct.BalancedParens
import HaskellWorks.Data.Bits.Broadword.Type
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "Data.Succinct.BalancedParens.FindCloseNSpec" $ do
  it "returns same result as broadword" $ requireProperty $ do
    w <- forAll $ G.word64 R.constantBounded
    p <- forAll $ G.word64 (R.linear 1 64)
    findClose w p === findClose (Broadword w) p
