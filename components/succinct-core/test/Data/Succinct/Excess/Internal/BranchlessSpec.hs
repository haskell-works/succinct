module Data.Succinct.Excess.Internal.BranchlessSpec (spec) where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Succinct.Excess.Internal.Branchless as BL
import qualified Hedgehog.Gen                                 as G
import qualified Hedgehog.Range                               as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "Data.Succinct.Excess.Internal.Branchless" $ do
  describe "For Int64" $ do
    it "minInt64 behaves like min" $ requireProperty $ do
      a <- forAll $ G.int64 R.constantBounded
      b <- forAll $ G.int64 R.constantBounded

      min a b === BL.minInt64 a b
    it "minInt behaves like min" $ requireProperty $ do
      a <- forAll $ G.int R.constantBounded
      b <- forAll $ G.int R.constantBounded

      min a b === BL.minInt a b
    it "maxInt64 behaves like max" $ requireProperty $ do
      a <- forAll $ G.int64 R.constantBounded
      b <- forAll $ G.int64 R.constantBounded

      max a b === BL.maxInt64 a b
    it "maxInt behaves like max" $ requireProperty $ do
      a <- forAll $ G.int R.constantBounded
      b <- forAll $ G.int R.constantBounded

      max a b === BL.maxInt a b
