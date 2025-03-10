{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}


module Data.Succinct.BalancedParens.RangeMin2Spec where

import Control.Monad                                (mfilter)
import Data.Word
import Data.Succinct.BalancedParens
import Data.Succinct.BalancedParens.RangeMin2
import HaskellWorks.Data.Bits.BitLength
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

factor :: Int
factor = 16384
{-# INLINE factor #-}

spec :: Spec
spec = describe "Data.Succinct.BalancedParens.RangeMinSpec2" $ do
  it "For a simple bit string can find close" $ requireTest $ do
    let v = fromBitTextByteString "11101111 10100101 01111110 10110010 10111011 10111011 00011111 11011100" :: DVS.Vector Word64
    let !rm = mkRangeMin2 v
    findClose rm 61 === findClose v 61
  it "findClose should return the same result" $ requireProperty $ do
    v <- forAll $ G.storableVector (R.linear 1 4) (G.word64 R.constantBounded)
    let !rm = mkRangeMin2 v
    let len = bitLength v
    [mfilter (<= bitLength v) (findClose rm i) | i <- [1..len]] === [mfilter (<= bitLength v) (findClose v i) | i <- [1..len]]
  it "findClose should return the same result over all counts" $ requireProperty $ do
    v <- forAll $ G.storableVector (R.linear 1 factor) (G.word64 R.constantBounded)
    p <- forAll $ G.count (R.linear 1 (bitLength v))
    let !rm = mkRangeMin2 v
    mfilter (<= bitLength v) (findClose rm p) === mfilter (<= bitLength v) (findClose v p)
  it "nextSibling should return the same result over all counts" $ requireProperty $ do
    v <- forAll $ G.storableVector (R.linear 1 factor) (G.word64 R.constantBounded)
    p <- forAll $ G.count (R.linear 1 (bitLength v))
    let !rm = mkRangeMin2 v
    nextSibling rm p === nextSibling v p
