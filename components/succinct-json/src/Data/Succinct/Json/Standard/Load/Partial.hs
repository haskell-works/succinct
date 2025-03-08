{-# LANGUAGE BangPatterns #-}

module Data.Succinct.Json.Standard.Load.Partial
  ( loadPartial
  , loadPartialWithCsPoppyIndex
  , loadPartialWithIndex
  ) where

import Data.Succinct.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import Data.Succinct.Json.Internal.PartialIndex
import Data.Succinct.Json.PartialValue
import Data.Succinct.Json.Standard.Cursor.Generic
import Data.Succinct.Json.Standard.Cursor.Load.Raw
import Data.Succinct.RankSelect.CsPoppy

loadPartialWithIndex :: String -> IO JsonPartialValue
loadPartialWithIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = GenericCursor jsonBS (BitShown jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadPartialWithCsPoppyIndex :: String -> IO JsonPartialValue
loadPartialWithCsPoppyIndex filename = do
  (jsonBS, jsonIb, jsonBp) <- loadRawWithIndex filename
  let cursor = GenericCursor jsonBS (makeCsPoppy jsonIb) (SimpleBalancedParens jsonBp) 1
  let !jsonResult = jsonPartialJsonValueAt (jsonPartialIndexAt cursor)
  return jsonResult

loadPartial :: String -> IO JsonPartialValue
loadPartial = loadPartialWithCsPoppyIndex
