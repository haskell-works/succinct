module Data.Succinct.Xml.Succinct.Cursor.Create
  ( byteStringAsFastCursor
  , byteStringAsSlowCursor
  ) where

import Data.Coerce
import Data.Succinct.BalancedParens.RangeMin2
import Data.Succinct.BalancedParens.Simple
import Data.Succinct.RankSelect.CsPoppy1
import Data.Succinct.Xml.Succinct.Cursor
import Data.Succinct.Xml.Succinct.Cursor.BlankedXml
import Data.Succinct.Xml.Succinct.Cursor.Types
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Vector.Storable

import qualified Data.ByteString                     as BS
import qualified Data.Succinct.Xml.Internal.ToIbBp64 as I

byteStringAsSlowCursor :: BS.ByteString -> SlowCursor
byteStringAsSlowCursor bs = XmlCursor
  { cursorText      = bs
  , interests       = BitShown ib
  , balancedParens  = SimpleBalancedParens bp
  , cursorRank      = 1
  }
  where blankedXml  = bsToBlankedXml bs
        bsLen       = BS.length bs
        idxLen      = (bsLen + 7) `div` 8
        (ib, bp)    = construct64UnzipN idxLen (I.toIbBp64 blankedXml)

byteStringAsFastCursor :: BS.ByteString -> FastCursor
byteStringAsFastCursor bs = XmlCursor bs ibCsPoppy rangeMinMax r
  where XmlCursor _ ib bp r = byteStringAsSlowCursor bs
        bpCsPoppy           = makeCsPoppy (coerce bp)
        rangeMinMax         = mkRangeMin2 bpCsPoppy
        ibCsPoppy           = makeCsPoppy (coerce ib)
