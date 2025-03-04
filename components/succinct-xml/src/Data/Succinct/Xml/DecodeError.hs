
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Succinct.Xml.DecodeError where

import Control.DeepSeq
import Data.Text       (Text)
import GHC.Generics

newtype DecodeError = DecodeError Text deriving (Eq, Show, Generic, NFData)
