module Data.Succinct.Json.Internal.Standard.Cursor.Token
  ( jsonTokenAt
  ) where

import Data.ByteString.Internal                                as BSI
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Drop
import Data.Succinct.Json.Internal.Standard.Token.Tokenize
import Data.Succinct.Json.Standard.Cursor.Generic
import HaskellWorks.Data.Positioning
import Data.Succinct.RankSelect.Base.Rank1
import Data.Succinct.RankSelect.Base.Select1
import Prelude                                                 hiding (drop)

import qualified Data.Attoparsec.ByteString.Char8 as ABC

jsonTokenAt :: (Rank1 w, Select1 v, TestBit w) => GenericCursor ByteString v w -> Maybe (JsonToken String Double)
jsonTokenAt k = if balancedParens k .?. lastPositionOf (cursorRank k)
  then case ABC.parse parseJsonToken (drop (toCount (jsonCursorPos k)) (cursorText k)) of
    ABC.Fail    {}  -> error "Failed to parse token in cursor"
    ABC.Partial _   -> error "Failed to parse token in cursor"
    ABC.Done    _ r -> Just r
  else Nothing
