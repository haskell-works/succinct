{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module App.Naive
  ( loadSlowCursor
  , loadFastCursor
  ) where

import Data.Succinct.BalancedParens.RangeMin2
import Data.Succinct.BalancedParens.Simple
import Data.Succinct.RankSelect.CsPoppy1
import Data.Succinct.Xml.Succinct.Cursor
import Data.Succinct.Xml.Succinct.Cursor.MMap
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString

import qualified Data.ByteString as BS

-- | Load an XML file into memory and return a raw cursor initialised to the
-- start of the XML document.
loadSlowCursor :: FilePath -> IO SlowCursor
loadSlowCursor path = do
  !bs <- BS.readFile path
  let !cursor = fromByteString bs :: SlowCursor
  return cursor

-- | Load an XML file into memory and return a query-optimised cursor initialised
-- to the start of the XML document.
loadFastCursor :: FilePath -> IO FastCursor
loadFastCursor filename = do
  -- Load the XML file into memory as a raw cursor.
  -- The raw XML data is `text`, and `ib` and `bp` are the indexes.
  -- `ib` and `bp` can be persisted to an index file for later use to avoid
  -- re-parsing the file.
  XmlCursor !text (BitShown !ib) (SimpleBalancedParens !bp) _ <- loadSlowCursor filename
  let !bpCsPoppy = makeCsPoppy bp
  let !rangeMinMax = mkRangeMin2 bpCsPoppy
  let !ibCsPoppy = makeCsPoppy ib
  return $ XmlCursor text ibCsPoppy rangeMinMax 1
