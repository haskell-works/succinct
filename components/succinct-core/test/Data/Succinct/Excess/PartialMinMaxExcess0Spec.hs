{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Data.Succinct.Excess.PartialMinMaxExcess0Spec (spec) where

import Data.Succinct.Excess.MinMaxExcess0
import Data.Succinct.Excess.PartialMinMaxExcess0
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "Data.Succinct.Excess.PartialMinMaxExcess0Spec" $ do
  describe "Equivalent to partialMinMaxExcess1 implementation" $ do
    it "For word8" $ requireProperty $ do
      w <- forAll $ G.word8 R.constantBounded
      minMaxExcess0 w === partialMinMaxExcess0 8 w
    it "For word16" $ requireProperty $ do
      w <- forAll $ G.word16 R.constantBounded
      minMaxExcess0 w === partialMinMaxExcess0 16 w
    it "For word32" $ requireProperty $ do
      w <- forAll $ G.word32 R.constantBounded
      minMaxExcess0 w === partialMinMaxExcess0 32 w
    it "For word64" $ requireProperty $ do
      w <- forAll $ G.word64 R.constantBounded
      minMaxExcess0 w === partialMinMaxExcess0 64 w
