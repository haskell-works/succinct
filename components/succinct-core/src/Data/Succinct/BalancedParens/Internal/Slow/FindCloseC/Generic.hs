module Data.Succinct.BalancedParens.Internal.Slow.FindCloseC.Generic
  ( findCloseC
  ) where

import Data.Succinct.BalancedParens.CloseAt
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning

import qualified Data.Succinct.BalancedParens.Internal.Slow.FindCloseN.Generic as G

-- | Find position closing parenthesis from beginning of bit string, carrying a nesting level of 'c'
findCloseC :: (BitLength a, CloseAt a, TestBit a) => a -> Count -> Maybe Count
findCloseC v c = G.findCloseN v c 0
{-# INLINE findCloseC #-}
