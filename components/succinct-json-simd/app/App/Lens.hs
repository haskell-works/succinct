{-# LANGUAGE TemplateHaskell #-}

module App.Lens where

import App.Commands.Types
import Control.Lens

makeFields ''CreateIndexOptions
