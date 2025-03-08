module Data.Succinct.Json.Standard.Cursor.Slow
  ( Cursor
  , fromBsIbBp
  , fromByteString
  , fromForeignRegion
  , fromString
  ) where

import Data.Word
import Foreign.ForeignPtr
import Data.Succinct.Json.Standard.Cursor.Generic
import Data.Succinct.Json.Standard.Cursor.IbBp
import Data.Succinct.Json.Standard.Cursor.Specific

import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Char8                   as BSC
import qualified Data.ByteString.Internal                as BSI
import qualified Data.Succinct.BalancedParens            as BP
import qualified Data.Succinct.Json.Standard.Cursor.IbBp as J
import qualified Data.Vector.Storable                    as DVS
import qualified HaskellWorks.Data.FromForeignRegion     as F

data Slow

instance SpecificCursor Slow where
  type CursorOf Slow = Cursor

type Cursor = GenericCursor BS.ByteString (DVS.Vector Word64) (BP.SimpleBalancedParens (DVS.Vector Word64))

fromBsIbBp :: BS.ByteString -> IbBp -> Cursor
fromBsIbBp bs ibBp = GenericCursor
  { cursorText      = bs
  , interests       = ibPart
  , balancedParens  = BP.SimpleBalancedParens bpPart
  , cursorRank      = 1
  }
  where J.IbBp ibPart bpPart = ibBp

fromByteString :: BS.ByteString -> Cursor
fromByteString bs = fromBsIbBp bs (J.slowToIbBp bs)

fromForeignRegion :: F.ForeignRegion -> Cursor
fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

fromString :: String -> Cursor
fromString = fromByteString . BSC.pack
