


module Data.Succinct.Simd.Comparison
  ( CmpEqWord8s(..)
  ) where

import Data.Succinct.Simd.Capabilities
import Data.Word

import qualified Data.Succinct.Simd.Comparison.Avx2  as AVX2
import qualified Data.Succinct.Simd.Comparison.Stock as STOCK
import qualified Data.Vector.Storable                as DVS

class CmpEqWord8s a where
  cmpEqWord8s :: Word8 -> a -> a

instance CmpEqWord8s (DVS.Vector Word64) where
  cmpEqWord8s w bs = if
    | avx2Enabled -> AVX2.cmpEqWord8s  w bs
    | True        -> STOCK.cmpEqWord8s w bs
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s (DVS.Vector Word8) where
  cmpEqWord8s w bs = if
    | avx2Enabled -> AVX2.cmpEqWord8s  w bs
    | True        -> STOCK.cmpEqWord8s w bs
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s [DVS.Vector Word64] where
  cmpEqWord8s w bs = if
    | avx2Enabled -> AVX2.cmpEqWord8s  w bs
    | True        -> STOCK.cmpEqWord8s w bs
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s [DVS.Vector Word8] where
  cmpEqWord8s w bs = if
    | avx2Enabled -> AVX2.cmpEqWord8s  w bs
    | True        -> STOCK.cmpEqWord8s w bs
  {-# INLINE cmpEqWord8s #-}
