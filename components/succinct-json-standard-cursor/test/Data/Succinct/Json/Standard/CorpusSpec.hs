{-# LANGUAGE BangPatterns              #-}





{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}


{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Succinct.Json.Standard.CorpusSpec(spec) where

import Control.Monad.IO.Class
import Data.Succinct.BalancedParens.Simple
import Data.Succinct.Json.Standard.Cursor.Generic
import HaskellWorks.Data.Bits.FromBitTextByteString
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import System.FilePath
import Test.Hspec

import qualified Data.ByteString                         as BS
import qualified Data.Succinct.Json.Standard.Cursor.Slow as SLOW

{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "Data.Succinct.Json.Corpus" $ do
  let base = "components/succinct-json/corpus"
  it "Corpus 5000B loads properly" $ requireTest $ do
    inJsonBS                    <- liftIO $ BS.readFile $ base </> "5000B.json"
    inInterestBitsBS            <- liftIO $ BS.readFile $ base </> "5000B.json.ib.idx"
    inInterestBalancedParensBS  <- liftIO $ BS.readFile $ base </> "5000B.json.bp.idx"
    let inInterestBits            = fromBitTextByteString inInterestBitsBS
    let inInterestBalancedParens  = fromBitTextByteString inInterestBalancedParensBS
    let !cursor = SLOW.fromByteString inJsonBS
    let text                    = cursorText      cursor
    let ib                      = interests       cursor
    let SimpleBalancedParens bp = balancedParens  cursor
    text === inJsonBS
    ib === inInterestBits
    bp === inInterestBalancedParens
  it "issue-0001 loads properly" $ requireTest $ do
    inJsonBS                    <- liftIO $ BS.readFile $ base </> "issue-0001.json"
    inInterestBitsBS            <- liftIO $ BS.readFile $ base </> "issue-0001.json.ib.idx"
    inInterestBalancedParensBS  <- liftIO $ BS.readFile $ base </> "issue-0001.json.bp.idx"
    let inInterestBits            = fromBitTextByteString inInterestBitsBS
    let inInterestBalancedParens  = fromBitTextByteString inInterestBalancedParensBS
    let !cursor = SLOW.fromByteString inJsonBS
    let text                    = cursorText      cursor
    let ib                      = interests       cursor
    let SimpleBalancedParens bp = balancedParens  cursor
    text === inJsonBS
    ib === inInterestBits
    bp === inInterestBalancedParens
