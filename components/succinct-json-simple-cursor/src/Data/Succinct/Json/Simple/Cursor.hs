





module Data.Succinct.Json.Simple.Cursor
  ( JsonCursor(..)
  , jsonCursorPos
  ) where

import Data.String
import Data.Succinct.RankSelect.Base.Rank0
import Data.Succinct.RankSelect.Base.Rank1
import Data.Succinct.RankSelect.Base.Select1
import Data.Succinct.RankSelect.CsPoppy
import Data.Word
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.TreeCursor
import Prelude                                   hiding (drop)

import qualified Data.ByteString                            as BS
import qualified Data.ByteString.Char8                      as BSC
import qualified Data.ByteString.Internal                   as BSI
import qualified Data.Vector.Storable                       as DVS
import qualified Foreign.ForeignPtr                         as F
import qualified Data.Succinct.BalancedParens               as BP
import qualified Data.Succinct.BalancedParens.RangeMin      as RM
import qualified Data.Succinct.Json.Simple.Cursor.SemiIndex as SI

data JsonCursor t v w = JsonCursor
  { cursorText     :: !t
  , interests      :: !v
  , balancedParens :: !w
  , cursorRank     :: !Count
  }
  deriving (Eq, Show)

instance FromByteString (JsonCursor BS.ByteString (DVS.Vector Word64) (BP.SimpleBalancedParens (DVS.Vector Word64))) where
  fromByteString bs = JsonCursor
    { cursorText      = bs
    , interests       = ib
    , balancedParens  = BP.SimpleBalancedParens bp
    , cursorRank      = 1
    }
    where SI.SemiIndex _ ib bp = SI.buildSemiIndex bs

instance FromByteString (JsonCursor BS.ByteString CsPoppy (RM.RangeMin CsPoppy)) where
  fromByteString bs = JsonCursor
    { cursorText      = bs
    , interests       = makeCsPoppy ib
    , balancedParens  = RM.mkRangeMin (makeCsPoppy bp)
    , cursorRank      = 1
    }
    where SI.SemiIndex _ ib bp = SI.buildSemiIndex bs

instance FromForeignRegion (JsonCursor BS.ByteString (DVS.Vector Word64) (BP.SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (F.castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString CsPoppy (RM.RangeMin CsPoppy)) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (F.castForeignPtr fptr) offset size)

instance IsString (JsonCursor BS.ByteString (DVS.Vector Word64) (BP.SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString CsPoppy (RM.RangeMin CsPoppy)) where
  fromString = fromByteString . BSC.pack

instance (BP.BalancedParens u, Rank1 u, Rank0 u) => TreeCursor (JsonCursor t v u) where
  firstChild :: JsonCursor t v u -> Maybe (JsonCursor t v u)
  firstChild k = let mq = BP.firstChild (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  nextSibling :: JsonCursor t v u -> Maybe (JsonCursor t v u)
  nextSibling k = (\q -> k { cursorRank = q }) <$> BP.nextSibling (balancedParens k) (cursorRank k)

  parent :: JsonCursor t v u -> Maybe (JsonCursor t v u)
  parent k = let mq = BP.parent (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  depth :: JsonCursor t v u -> Maybe Count
  depth k = BP.depth (balancedParens k) (cursorRank k)

  subtreeSize :: JsonCursor t v u -> Maybe Count
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

jsonCursorPos :: (Rank1 w, Select1 v) => JsonCursor s v w -> Position
jsonCursorPos k = toPosition (select1 ik (rank1 bpk (cursorRank k)) - 1)
  where ik  = interests k
        bpk = balancedParens k
