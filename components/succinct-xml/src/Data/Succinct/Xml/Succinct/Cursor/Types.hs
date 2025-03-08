module Data.Succinct.Xml.Succinct.Cursor.Types
  ( SlowCursor
  , FastCursor
  ) where

import Data.Succinct.BalancedParens.RangeMin2
import Data.Succinct.BalancedParens.Simple
import Data.Succinct.RankSelect.CsPoppy1
import Data.Succinct.Xml.Succinct.Cursor
import Data.Word
import HaskellWorks.Data.Bits.BitShown

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

type SlowCursor = XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))

type FastCursor = XmlCursor BS.ByteString CsPoppy1 (RangeMin2 CsPoppy1)
