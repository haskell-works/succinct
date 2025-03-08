module Data.Succinct.Json.Simple.Cursor.Fast
  ( fromByteString
  , fromForeignRegion
  , fromString
  ) where

import Foreign.ForeignPtr
import Data.Succinct.Json.Simple.Cursor
import Data.Succinct.RankSelect.CsPoppy

import qualified Data.ByteString                                  as BS
import qualified Data.ByteString.Char8                            as BSC
import qualified Data.ByteString.Internal                         as BSI
import qualified Data.Succinct.BalancedParens.RangeMin            as RM
import qualified Data.Succinct.Json.Simple.Cursor.Internal.IbBp   as J
import qualified Data.Succinct.Json.Simple.Cursor.Internal.ToIbBp as J
import qualified HaskellWorks.Data.FromForeignRegion              as F

fromByteString :: BS.ByteString -> JsonCursor BS.ByteString CsPoppy (RM.RangeMin CsPoppy)
fromByteString bs = JsonCursor
  { cursorText      = bs
  , interests       = makeCsPoppy ib
  , balancedParens  = RM.mkRangeMin (makeCsPoppy bp)
  , cursorRank      = 1
  }
  where J.IbBp ib bp = J.toIbBp bs

fromForeignRegion :: F.ForeignRegion -> JsonCursor BS.ByteString CsPoppy (RM.RangeMin CsPoppy)
fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

fromString :: String -> JsonCursor BS.ByteString CsPoppy (RM.RangeMin CsPoppy)
fromString = fromByteString . BSC.pack
