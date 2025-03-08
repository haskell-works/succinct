{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Data.Succinct.BalancedParens.Internal.BroadwordSpec where

import Data.Word
import GHC.Generics
import Data.Succinct.BalancedParens.FindClose
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.Broadword.Type
import HaskellWorks.Data.Bits.FromBitTextByteString
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Vector.Storable             as DVS
import qualified Data.Succinct.BalancedParens.Gen as G
import qualified Hedgehog.Gen                     as G
import qualified Hedgehog.Range                   as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}
{- HLINT ignore "Reduce duplication"  -}

newtype ShowVector a = ShowVector a deriving (Eq, BitShow, Generic)

instance BitShow a => Show (ShowVector a) where
  show = bitShow

spec :: Spec
spec = describe "Data.Succinct.BalancedParens.BroadwordSpec" $ do
  describe "For (()(()())) 1101101000" $ do
    let bs = Broadword (91 :: Word64)
    it "Test 1a" $ requireTest $ findClose bs  1 === Just 10
    it "Test 1b" $ requireTest $ findClose bs  2 === Just  3
    it "Test 1b" $ requireTest $ findClose bs  3 === Just  3
    it "Test 1b" $ requireTest $ findClose bs  4 === Just  9
    it "Test 1b" $ requireTest $ findClose bs  5 === Just  6
    it "Test 1b" $ requireTest $ findClose bs  6 === Just  6
    it "Test 1b" $ requireTest $ findClose bs  7 === Just  8
    it "Test 1b" $ requireTest $ findClose bs  8 === Just  8
    it "Test 1b" $ requireTest $ findClose bs  9 === Just  9
    it "Test 1b" $ requireTest $ findClose bs 10 === Just 10
    -- it "Test 2a" $ requireTest $ findOpen  bs 10 === Just  1
    -- it "Test 2b" $ requireTest $ findOpen  bs  3 === Just  2
    -- it "Test 3a" $ requireTest $ enclose   bs  2 === Just  1
    -- it "Test 3b" $ requireTest $ enclose   bs  7 === Just  4
  describe "For (()(()())) 11011010 00000000 :: DVS.Vector Word8" $ do
    let bs = Broadword (DVS.head (fromBitTextByteString "11011010 00000000") :: Word64)
    it "Test 1a" $ requireTest $ findClose bs  1 === Just 10
    it "Test 1b" $ requireTest $ findClose bs  2 === Just  3
    it "Test 1b" $ requireTest $ findClose bs  3 === Just  3
    it "Test 1b" $ requireTest $ findClose bs  4 === Just  9
    it "Test 1b" $ requireTest $ findClose bs  5 === Just  6
    it "Test 1b" $ requireTest $ findClose bs  6 === Just  6
    it "Test 1b" $ requireTest $ findClose bs  7 === Just  8
    it "Test 1b" $ requireTest $ findClose bs  8 === Just  8
    it "Test 1b" $ requireTest $ findClose bs  9 === Just  9
    it "Test 1b" $ requireTest $ findClose bs 10 === Just 10
    -- it "Test 2a" $ requireTest $ findOpen  bs 10 === Just  1
    -- it "Test 2b" $ requireTest $ findOpen  bs  3 === Just  2
    -- it "Test 3a" $ requireTest $ enclose   bs  2 === Just  1
    -- it "Test 3b" $ requireTest $ enclose   bs  7 === Just  4
    -- it "firstChild 1"  $ requireTest $ firstChild  bs 1 === Just 2
    -- it "firstChild 4"  $ requireTest $ firstChild  bs 4 === Just 5
    -- it "nextSibling 2" $ requireTest $ nextSibling bs 2 === Just 4
    -- it "nextSibling 5" $ requireTest $ nextSibling bs 5 === Just 7
    -- it "parent 2" $ requireTest $ parent bs 2 === Just 1
    -- it "parent 5" $ requireTest $ parent bs 5 === Just 4
    -- it "depth  1" $ requireTest $ depth bs  1 === Just 1
    -- it "depth  2" $ requireTest $ depth bs  2 === Just 2
    -- it "depth  3" $ requireTest $ depth bs  3 === Just 2
    -- it "depth  4" $ requireTest $ depth bs  4 === Just 2
    -- it "depth  5" $ requireTest $ depth bs  5 === Just 3
    -- it "depth  6" $ requireTest $ depth bs  6 === Just 3
    -- it "depth  7" $ requireTest $ depth bs  7 === Just 3
    -- it "depth  8" $ requireTest $ depth bs  8 === Just 3
    -- it "depth  9" $ requireTest $ depth bs  9 === Just 2
    -- it "depth 10" $ requireTest $ depth bs 10 === Just 1
    -- it "subtreeSize  1" $ requireTest $ subtreeSize bs  1 === Just 5
    -- it "subtreeSize  2" $ requireTest $ subtreeSize bs  2 === Just 1
    -- it "subtreeSize  3" $ requireTest $ subtreeSize bs  3 === Just 0
    -- it "subtreeSize  4" $ requireTest $ subtreeSize bs  4 === Just 3
    -- it "subtreeSize  5" $ requireTest $ subtreeSize bs  5 === Just 1
    -- it "subtreeSize  6" $ requireTest $ subtreeSize bs  6 === Just 0
    -- it "subtreeSize  7" $ requireTest $ subtreeSize bs  7 === Just 1
    -- it "subtreeSize  8" $ requireTest $ subtreeSize bs  8 === Just 0
    -- it "subtreeSize  9" $ requireTest $ subtreeSize bs  9 === Just 0
    -- it "subtreeSize 10" $ requireTest $ subtreeSize bs 10 === Just 0
  -- describe "Does not suffer exceptions" $ do
  --   it "when calling nextSibling from valid locations" $ do
  --     forAll (vectorSizedBetween 1 64) $ \(ShowVector v) -> do
  --       [nextSibling v p | p <- [1..bitLength v]] === [nextSibling v p | p <- [1..bitLength v]]
  it "Broadword findClose should behave the same as Naive findClose" $ requireProperty $ do
    w <- forAll $ G.word64 R.constantBounded
    p <- forAll $ G.count (R.linear 1 64)
    findClose w p === findClose (Broadword w) p
