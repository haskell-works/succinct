


{-# LANGUAGE TemplateHaskell        #-}

module Data.Succinct.Model.Lens where

import Control.Lens
import Data.Succinct.Model.Type

makeFields ''Storage
makeFields ''Mount
