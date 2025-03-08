module Data.Succinct.Json.Standard.Cursor.Internal.ToInterestBits64
  ( ToInterestBits64(..)
  ) where

import Control.Applicative
import Data.ByteString.Internal
import Data.Succinct.Json.Standard.Cursor.Internal.MakeIndex
import Data.Word

import qualified Data.ByteString                                         as BS
import qualified Data.Succinct.Json.Standard.Cursor.Internal.BlankedJson as J
import qualified Data.Vector.Storable                                    as DVS

class ToInterestBits64 a where
  toInterestBits64 :: a -> DVS.Vector Word64

instance ToInterestBits64 J.BlankedJson where
  toInterestBits64 bj = DVS.unsafeCast (DVS.unfoldrN newLen genInterestForever interestBS)
    where interestBS    = blankedJsonBssToInterestBitsBs (J.unBlankedJson bj)
          newLen        = (BS.length interestBS + 7) `div` 8 * 8

blankedJsonBssToInterestBitsBs :: [ByteString] -> ByteString
blankedJsonBssToInterestBitsBs bss = BS.concat $ blankedJsonToInterestBits bss

genInterestForever :: ByteString -> Maybe (Word8, ByteString)
genInterestForever bs = BS.uncons bs <|> Just (0, bs)
