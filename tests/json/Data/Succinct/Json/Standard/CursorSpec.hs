{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Succinct.Json.Standard.CursorSpec(spec) where

import Data.Succinct.Json.Standard.GenCursorTest
import Test.Hspec

import qualified HaskellWorks.Data.Json.Standard.Cursor.Fast as FAST
import qualified HaskellWorks.Data.Json.Standard.Cursor.Slow as SLOW

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant bracket"  -}
{- HLINT ignore "Redundant do"       -}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  genTest "DVS.Vector Word64" SLOW.fromString
  genTest "CsPoppy"           FAST.fromString
