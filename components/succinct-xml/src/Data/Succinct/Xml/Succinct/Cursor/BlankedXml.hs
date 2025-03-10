{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Succinct.Xml.Succinct.Cursor.BlankedXml
  ( BlankedXml(..)
  , FromBlankedXml(..)
  , getBlankedXml
  , bsToBlankedXml
  , lbsToBlankedXml
  ) where

import Control.DeepSeq
import Data.Succinct.Xml.Internal.Blank
import GHC.Generics

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

newtype BlankedXml = BlankedXml
  { unblankedXml :: [BS.ByteString]
  } deriving (Eq, Show, Generic, NFData)

getBlankedXml :: BlankedXml -> [BS.ByteString]
getBlankedXml (BlankedXml bs) = bs

class FromBlankedXml a where
  fromBlankedXml :: BlankedXml -> a

bsToBlankedXml :: BS.ByteString -> BlankedXml
bsToBlankedXml bs = BlankedXml (blankXml [bs])

lbsToBlankedXml :: LBS.ByteString -> BlankedXml
lbsToBlankedXml lbs = BlankedXml (blankXml (LBS.toChunks lbs))
