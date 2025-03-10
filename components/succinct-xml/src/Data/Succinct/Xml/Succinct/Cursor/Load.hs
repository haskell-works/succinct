module Data.Succinct.Xml.Succinct.Cursor.Load
  ( loadSlowCursor
  , loadFastCursor
  ) where

import Data.Succinct.Xml.Succinct.Cursor.Create
import Data.Succinct.Xml.Succinct.Cursor.Types

import qualified Data.ByteString as BS

-- | Load an XML file into memory and return a raw cursor initialised to the
-- start of the XML document.
loadSlowCursor :: FilePath -> IO SlowCursor
loadSlowCursor = fmap byteStringAsSlowCursor . BS.readFile

-- | Load an XML file into memory and return a query-optimised cursor initialised
-- to the start of the XML document.
loadFastCursor :: FilePath -> IO FastCursor
loadFastCursor = fmap byteStringAsFastCursor . BS.readFile
