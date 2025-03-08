{-# LANGUAGE OverloadedStrings #-}

module Data.Succinct.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word8Spec where

import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Succinct.BalancedParens.FindClose                                      as C
import qualified Data.Succinct.BalancedParens.Internal.Broadword.FindClose.Word8             as BW8
import qualified Data.Succinct.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word8 as BW8
import qualified Data.Succinct.BalancedParens.Internal.Slow.FindUnmatchedCloseFar.Word8      as SW8
import qualified Hedgehog.Gen                                                                as G
import qualified Hedgehog.Range                                                              as R

{- HLINT ignore "Evaluate"            -}
{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "Data.Succinct.BalancedParens.Broadword.Word8Spec" $ do
  it "findUnmatchedCloseFar" $ require $ withTests 1000 $ property $ do
    c <- forAll $ G.word64 (R.linear 0 64)
    p <- forAll $ G.word64 (R.linear 0 8)
    w <- forAll $ G.word8 R.constantBounded
    annotateShow $ bitShow w
    BW8.findUnmatchedCloseFar c p w === SW8.findUnmatchedCloseFar c p w
  it "findClose" $ require $ withTests 1000 $ property $ do
    p <- forAll $ G.word64 (R.linear 1 128)
    w <- forAll $ G.word8 R.constantBounded
    annotateShow $ bitShow w
    BW8.findClose w p === C.findClose w p
