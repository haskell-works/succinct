{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Succinct.Json.Standard.CursorSpec(spec) where

import Data.Succinct.Json.Standard.GenCursorTest
import Test.Hspec

import qualified Data.Succinct.Json.Standard.Cursor.Fast as FAST
import qualified Data.Succinct.Json.Standard.Cursor.Slow as SLOW

{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "Data.Succinct.Json.Succinct.CursorSpec" $ do
  genTest "DVS.Vector Word64" SLOW.fromString
  genTest "CsPoppy1"          FAST.fromString
