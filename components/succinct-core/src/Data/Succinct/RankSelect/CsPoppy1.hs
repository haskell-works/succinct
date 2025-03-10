{-# OPTIONS_GHC-funbox-strict-fields #-}

{-# LANGUAGE BangPatterns   #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Rank2Types     #-}

module Data.Succinct.RankSelect.CsPoppy1
    ( CsPoppy1(..)
    , makeCsPoppy
    ) where

import Control.DeepSeq
import Data.Succinct.BalancedParens.BalancedParens
import Data.Succinct.BalancedParens.CloseAt
import Data.Succinct.BalancedParens.Enclose
import Data.Succinct.BalancedParens.FindClose
import Data.Succinct.BalancedParens.FindCloseN
import Data.Succinct.BalancedParens.FindOpen
import Data.Succinct.BalancedParens.FindOpenN
import Data.Succinct.BalancedParens.NewCloseAt
import Data.Succinct.BalancedParens.OpenAt
import Data.Succinct.RankSelect.Base.Rank0
import Data.Succinct.RankSelect.Base.Rank1
import Data.Succinct.RankSelect.Base.Select1
import Data.Succinct.RankSelect.CsPoppy.Internal.CsInterleaved
import Data.Succinct.RankSelect.CsPoppy.Internal.Vector
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Vector.AsVector64
import Prelude                                                     hiding (drop, length, pi, take)

import qualified Data.Succinct.RankSelect.CsPoppy.Internal.Alpha1 as A1
import qualified Data.Vector.Storable                             as DVS

data CsPoppy1 = CsPoppy1
  { csPoppy1Bits   :: !(DVS.Vector Word64)
  , csPoppy1Index1 :: !A1.CsPoppyIndex
  } deriving (Eq, Show, NFData, Generic)

instance FromForeignRegion CsPoppy1 where
  fromForeignRegion = makeCsPoppy . fromForeignRegion

instance AsVector64 CsPoppy1 where
  asVector64 = asVector64 . csPoppy1Bits
  {-# INLINE asVector64 #-}

instance BitLength CsPoppy1 where
  bitLength = bitLength . csPoppy1Bits
  {-# INLINE bitLength #-}

instance PopCount1 CsPoppy1 where
  popCount1 v = getCsiTotal (CsInterleaved (lastOrZero (A1.csPoppyLayerM (csPoppy1Index1 v))))
  {-# INLINE popCount1 #-}

makeCsPoppy :: DVS.Vector Word64 -> CsPoppy1
makeCsPoppy v = CsPoppy1
  { csPoppy1Bits   = v
  , csPoppy1Index1 = A1.makeCsPoppyIndex v
  }

instance TestBit CsPoppy1 where
  (.?.) = (.?.) . csPoppy1Bits
  {-# INLINE (.?.) #-}

instance BitRead CsPoppy1 where
  bitRead = fmap makeCsPoppy . bitRead

instance Rank0 CsPoppy1 where
  rank0 rsbs p = p - rank1 rsbs p
  {-# INLINE rank0 #-}

instance Rank1 CsPoppy1 where
  rank1 (CsPoppy1 !v i) = A1.rank1On v i
  {-# INLINE rank1 #-}

instance Select1 CsPoppy1 where
  select1 (CsPoppy1 !v i) = A1.select1On v i
  {-# INLINE select1 #-}

instance OpenAt CsPoppy1 where
  openAt = openAt . csPoppy1Bits
  {-# INLINE openAt #-}

instance CloseAt CsPoppy1 where
  closeAt = closeAt . csPoppy1Bits
  {-# INLINE closeAt #-}

instance NewCloseAt CsPoppy1 where
  newCloseAt = newCloseAt . csPoppy1Bits
  {-# INLINE newCloseAt #-}

instance FindOpenN CsPoppy1 where
  findOpenN = findOpenN . csPoppy1Bits
  {-# INLINE findOpenN #-}

instance FindOpen CsPoppy1 where
  findOpen = findOpen . csPoppy1Bits
  {-# INLINE findOpen #-}

instance FindClose CsPoppy1 where
  findClose = findClose . csPoppy1Bits
  {-# INLINE findClose #-}

instance FindCloseN CsPoppy1 where
  findCloseN = findCloseN . csPoppy1Bits
  {-# INLINE findCloseN #-}

instance Enclose CsPoppy1 where
  enclose = enclose . csPoppy1Bits
  {-# INLINE enclose #-}

instance BalancedParens CsPoppy1 where
  firstChild  = firstChild  . csPoppy1Bits
  nextSibling = nextSibling . csPoppy1Bits
  parent      = parent      . csPoppy1Bits
  {-# INLINE firstChild  #-}
  {-# INLINE nextSibling #-}
  {-# INLINE parent      #-}
