module App.Cli.Options.Type where

import Data.Word
import GHC.Generics

data IndexType = CsPoppy | Poppy512 deriving (Eq, Read, Show, Generic)

newtype BuildOptions = BuildOptions
  { indexType :: IndexType
  } deriving (Eq, Show, Generic)

newtype SelectAllOptions = SelectAllOptions
  { indexType :: IndexType
  } deriving (Eq, Show, Generic)

newtype UnitTestOptions = UnitTestOptions
  { name :: String
  } deriving (Eq, Show, Generic)

data ValidateOptions = ValidateOptions
  { indexType :: IndexType
  , file      :: FilePath
  } deriving (Eq, Show, Generic)

newtype ValidateState = ValidateState
  { cumulativePopCount :: Word64
  } deriving (Eq, Show, Generic)

emptyValidateState :: ValidateState
emptyValidateState = ValidateState 0

newtype PositionsOptions = PositionsOptions
  { inputFile :: FilePath
  } deriving (Eq, Show, Generic)

data BitsToParensOptions = BitsToParensOptions
  { inputFile  :: FilePath
  , outputFile :: FilePath
  } deriving (Eq, Show, Generic)

data ParensToBitsOptions = ParensToBitsOptions
  { inputFile  :: FilePath
  , outputFile :: FilePath
  } deriving (Eq, Show, Generic)
