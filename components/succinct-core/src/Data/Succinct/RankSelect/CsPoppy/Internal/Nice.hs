module Data.Succinct.RankSelect.CsPoppy.Internal.Nice where

import Data.Succinct.RankSelect.CsPoppy
import Data.Succinct.RankSelect.CsPoppy.Internal.CsInterleaved
import HaskellWorks.Data.Bits.BitShow

import qualified Data.Succinct.RankSelect.CsPoppy.Internal.Alpha0 as A0
import qualified Data.Succinct.RankSelect.CsPoppy.Internal.Alpha1 as A1
import qualified Data.Vector.Storable                             as DVS

newtype Nice a = Nice a deriving Eq

instance Show (Nice CsPoppy) where
  showsPrec _ (Nice rsbs) = showString "CsPoppy "
    <> showString "{ csPoppyBits = "   <> shows (bitShow <$> DVS.toList (csPoppyBits     rsbs))
    <> showString ", csPoppyIndex0 = CsPoppyIndex"
    <> showString "{ csPoppyLayerM = " <> shows (CsInterleaved <$> DVS.toList (A0.csPoppyLayerM (csPoppyIndex0 rsbs)))
    <> showString ", csPoppyLayerS = " <> shows (A0.csPoppyLayerS (csPoppyIndex0 rsbs))
    <> showString " }"
    <> showString ", csPoppyIndex1 = CsPoppyIndex"
    <> showString "{ csPoppyLayerM = " <> shows (CsInterleaved <$> DVS.toList (A1.csPoppyLayerM (csPoppyIndex1 rsbs)))
    <> showString ", csPoppyLayerS = " <> shows (A1.csPoppyLayerS (csPoppyIndex1 rsbs))
    <> showString " }"
    <> showString " }"
