

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Succinct.BalancedParens.Simple
  ( SimpleBalancedParens(..)
  ) where

import Control.Monad
import GHC.Generics
import Data.Succinct.BalancedParens.BalancedParens
import Data.Succinct.BalancedParens.CloseAt
import Data.Succinct.BalancedParens.Enclose
import Data.Succinct.BalancedParens.FindClose
import Data.Succinct.BalancedParens.FindOpen
import Data.Succinct.BalancedParens.OpenAt
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import Data.Succinct.RankSelect.Base.Rank0
import Data.Succinct.RankSelect.Base.Rank1
import Data.Succinct.RankSelect.Base.Select0
import Data.Succinct.RankSelect.Base.Select1
import Prelude                                         as P

newtype SimpleBalancedParens a = SimpleBalancedParens a
  deriving (BalancedParens, FindOpen, FindClose, Enclose, OpenAt, CloseAt, BitLength, BitShow, Eq, Rank0, Rank1, Select0, Select1, TestBit, Generic)

instance Functor SimpleBalancedParens where
  fmap f (SimpleBalancedParens a) = SimpleBalancedParens (f a)
  {-# INLINABLE fmap   #-}

instance BitShow a => Show (SimpleBalancedParens a) where
  show = bitShow
