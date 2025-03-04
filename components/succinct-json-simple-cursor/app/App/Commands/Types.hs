


module App.Commands.Types
  ( CreateIndexOptions(..)
  ) where

import GHC.Generics

data CreateIndexOptions = CreateIndexOptions
  { filePath     :: FilePath
  , backend      :: String
  , outputIbFile :: Maybe FilePath
  , outputBpFile :: Maybe FilePath
  } deriving (Eq, Show, Generic)
