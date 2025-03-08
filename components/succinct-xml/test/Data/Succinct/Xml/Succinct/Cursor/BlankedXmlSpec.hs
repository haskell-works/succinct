{-# LANGUAGE OverloadedStrings #-}

module Data.Succinct.Xml.Succinct.Cursor.BlankedXmlSpec
  ( spec
  ) where

import Data.Succinct.Xml.Succinct.Cursor.BlankedXml
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "Data.Succinct.Xml.Succinct.Cursor.BlankedXmlSpec" $ do
  describe "Blanking XML should work" $ do
    it "on strict bytestrings" $ requireTest $ do
      let input       = "<attack><instances/></attack>"
      let expected    = "<       <          >        >"
      let blankedXml  = bsToBlankedXml input

      mconcat (unblankedXml blankedXml) === expected

    it "on lazy bytestrings" $ requireTest $ do
      let input       = "<attack><instances/></attack>"
      let expected    = "<       <          >        >"
      let blankedXml  = lbsToBlankedXml input

      mconcat (unblankedXml blankedXml) === expected
