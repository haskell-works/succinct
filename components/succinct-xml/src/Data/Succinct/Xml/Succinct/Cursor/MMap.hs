{-# LANGUAGE BangPatterns        #-}

module Data.Succinct.Xml.Succinct.Cursor.MMap
  ( SlowCursor
  , FastCursor
  , mmapSlowCursor
  , mmapFastCursor
  ) where

import Data.Succinct.BalancedParens.RangeMin2
import Data.Succinct.BalancedParens.Simple
import Data.Succinct.RankSelect.CsPoppy1
import Data.Succinct.Xml.Succinct.Cursor
import Data.Succinct.Xml.Succinct.Cursor.BlankedXml
import Data.Succinct.Xml.Succinct.Cursor.Types
import Data.Word
import Foreign.ForeignPtr
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Vector.Storable

import qualified Data.ByteString.Internal            as BSI
import qualified Data.Succinct.Xml.Internal.ToIbBp64 as I
import qualified System.IO.MMap                      as IO

mmapSlowCursor :: FilePath -> IO SlowCursor
mmapSlowCursor filePath = do
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let blankedXml = bsToBlankedXml bs
  let (ib, bp) = construct64UnzipN (fromIntegral size) (I.toIbBp64 blankedXml)
  let !cursor = XmlCursor
        { cursorText      = bs
        , interests       = BitShown ib
        , balancedParens  = SimpleBalancedParens bp
        , cursorRank      = 1
        }

  return cursor

mmapFastCursor :: FilePath -> IO FastCursor
mmapFastCursor filename = do
  -- Load the XML file into memory as a raw cursor.
  -- The raw XML data is `text`, and `ib` and `bp` are the indexes.
  -- `ib` and `bp` can be persisted to an index file for later use to avoid
  -- re-parsing the file.
  XmlCursor !text (BitShown !ib) (SimpleBalancedParens !bp) _ <- mmapSlowCursor filename
  let !bpCsPoppy = makeCsPoppy bp
  let !rangeMinMax = mkRangeMin2 bpCsPoppy
  let !ibCsPoppy = makeCsPoppy ib
  return $ XmlCursor text ibCsPoppy rangeMinMax 1
