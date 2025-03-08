module Data.Succinct.RankSelect.Base.Rank
  ( -- * Rank & Select
    Rank(..)
  ) where

import Data.Succinct.RankSelect.Base.Rank0
import Data.Succinct.RankSelect.Base.Rank1
import HaskellWorks.Data.Positioning

class Eq a => Rank v a where
  -- | Find the number of occurences of the given symbol in the prefix of the supplied bitstring of the given length
  rank
    :: a      -- ^ The symbol
    -> v      -- ^ The bitstring
    -> Count  -- ^ The prefix length
    -> Count

instance Rank [Bool] Bool where
  rank a = if a then rank1 else rank0
  {-# INLINABLE rank #-}
