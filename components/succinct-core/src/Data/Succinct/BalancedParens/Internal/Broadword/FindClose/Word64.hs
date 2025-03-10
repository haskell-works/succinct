module Data.Succinct.BalancedParens.Internal.Broadword.FindClose.Word64
  ( findClose
  ) where

import Data.Word
import Data.Succinct.BalancedParens.CloseAt
import HaskellWorks.Data.Positioning

import qualified Data.Succinct.BalancedParens.Internal.Broadword.FindUnmatchedCloseFar.Word64 as W64

-- | Find the position of the matching close parenthesis.
--
-- The position argument and return value is one-based.
--
-- If the parenthesis at the input position is an a close, then that is considered the
-- matching close parenthesis.
--
-- >>> import HaskellWorks.Data.Bits.BitRead
-- >>> import Data.Maybe
--
-- The following scans for the matching close parenthesis for the open parenthesis at position 1:
--
-- >>> findClose (fromJust $ bitRead "10000000") 1
-- Just 2
--
-- >>> findClose (fromJust $ bitRead "11000000") 1
-- Just 4
--
-- >>> findClose (fromJust $ bitRead "11010000") 1
-- Just 6
--
-- The following scans for the matching close parenthesis for the open parenthesis at position 2:
--
-- >>> findClose (fromJust $ bitRead "11010000") 2
-- Just 3
--
-- If the input position has a close parenthesis, then that position is returned:
--
-- >>> findClose (fromJust $ bitRead "11010000") 3
-- Just 3
--
-- The scan can continue past the end of the input word because every bit after then end of the
-- word is considered to be zero, which is a closing parenthesis:
--
-- >>> findClose (fromJust $ bitRead "11111110") 1
-- Just 14
findClose :: Word64 -> Count -> Maybe Count
findClose v p = if p > 0
  then if closeAt v p
    then Just p
    else Just (W64.findUnmatchedCloseFar 0 p v + 1)
  else Just (W64.findUnmatchedCloseFar 1 p v)
{-# INLINE findClose #-}
