module Data.Succinct.Xml.RawDecode where

import Data.Succinct.Xml.RawValue

class RawDecode a where
  rawDecode :: RawValue -> a

instance RawDecode RawValue where
  rawDecode = id
  {-# INLINE rawDecode #-}
