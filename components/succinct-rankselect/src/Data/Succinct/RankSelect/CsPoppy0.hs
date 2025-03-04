{-# OPTIONS_GHC-funbox-strict-fields #-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}




{-# LANGUAGE Rank2Types          #-}


module Data.Succinct.RankSelect.CsPoppy0
    ( CsPoppy0(..)
    , makeCsPoppy0
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
import Data.Succinct.RankSelect.Base.Select0
import Data.Succinct.RankSelect.CsPoppy.Internal.CsInterleaved
import Data.Succinct.RankSelect.CsPoppy.Internal.Vector
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount0
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Vector.AsVector64
import Prelude                                                     hiding (drop, length, pi, take)

import qualified Data.Succinct.RankSelect.CsPoppy.Internal.Alpha0 as A0
import qualified Data.Vector.Storable                             as DVS

data CsPoppy0 = CsPoppy0
  { csPoppy0Bits   :: !(DVS.Vector Word64)
  , csPoppy0Index0 :: !A0.CsPoppyIndex
  } deriving (Eq, Show, NFData, Generic)

instance FromForeignRegion CsPoppy0 where
  fromForeignRegion = makeCsPoppy0 . fromForeignRegion

instance AsVector64 CsPoppy0 where
  asVector64 = asVector64 . csPoppy0Bits
  {-# INLINE asVector64 #-}

instance BitLength CsPoppy0 where
  bitLength = bitLength . csPoppy0Bits
  {-# INLINE bitLength #-}

instance PopCount0 CsPoppy0 where
  popCount0 v = getCsiTotal (CsInterleaved (lastOrZero (A0.csPoppyLayerM (csPoppy0Index0 v))))
  {-# INLINE popCount0 #-}

makeCsPoppy0 :: DVS.Vector Word64 -> CsPoppy0
makeCsPoppy0 v = CsPoppy0
  { csPoppy0Bits   = v
  , csPoppy0Index0 = A0.makeCsPoppyIndex v
  }

instance TestBit CsPoppy0 where
  (.?.) = (.?.) . csPoppy0Bits
  {-# INLINE (.?.) #-}

instance BitRead CsPoppy0 where
  bitRead = fmap makeCsPoppy0 . bitRead

instance Rank1 CsPoppy0 where
  rank1 rsbs p = p - rank0 rsbs p
  {-# INLINE rank1 #-}

instance Rank0 CsPoppy0 where
  rank0 (CsPoppy0 !v i) = A0.rank0On v i
  {-# INLINE rank0 #-}

instance Select0 CsPoppy0 where
  select0 (CsPoppy0 !v i) = A0.select0On v i
  {-# INLINE select0 #-}

instance OpenAt CsPoppy0 where
  openAt = openAt . csPoppy0Bits
  {-# INLINE openAt #-}

instance CloseAt CsPoppy0 where
  closeAt = closeAt . csPoppy0Bits
  {-# INLINE closeAt #-}

instance NewCloseAt CsPoppy0 where
  newCloseAt = newCloseAt . csPoppy0Bits
  {-# INLINE newCloseAt #-}

instance FindOpenN CsPoppy0 where
  findOpenN = findOpenN . csPoppy0Bits
  {-# INLINE findOpenN #-}

instance FindOpen CsPoppy0 where
  findOpen = findOpen . csPoppy0Bits
  {-# INLINE findOpen #-}

instance FindClose CsPoppy0 where
  findClose = findClose . csPoppy0Bits
  {-# INLINE findClose #-}

instance FindCloseN CsPoppy0 where
  findCloseN = findCloseN . csPoppy0Bits
  {-# INLINE findCloseN #-}

instance Enclose CsPoppy0 where
  enclose = enclose . csPoppy0Bits
  {-# INLINE enclose #-}

instance BalancedParens CsPoppy0 where
  firstChild  = firstChild  . csPoppy0Bits
  nextSibling = nextSibling . csPoppy0Bits
  parent      = parent      . csPoppy0Bits
  {-# INLINE firstChild  #-}
  {-# INLINE nextSibling #-}
  {-# INLINE parent      #-}
