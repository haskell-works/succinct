{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE OverloadedStrings    #-}

module App.Commands.Demo
  ( cmdDemo
  ) where

import Data.Foldable
import Data.Maybe
import Data.Succinct.Xml.Decode
import Data.Succinct.Xml.DecodeResult
import Data.Succinct.Xml.RawDecode
import Data.Succinct.Xml.RawValue
import Data.Succinct.Xml.Succinct.Cursor.Load
import Data.Succinct.Xml.Succinct.Index
import Data.Succinct.Xml.Value
import Data.Text                                  (Text)
import HaskellWorks.Data.TreeCursor
import Options.Applicative                        hiding (columns)

import qualified App.Commands.Types as Z

-- | Parse the text of an XML node.
class ParseText a where
  parseText :: Value -> DecodeResult a

instance ParseText Text where
  parseText (XmlText text)      = DecodeOk text
  parseText (XmlCData text)     = DecodeOk text
  parseText (XmlElement _ _ cs) = DecodeOk $ mconcat $ mconcat $ toList . parseText <$> cs
  parseText _                   = DecodeOk ""

-- | Convert a decode result to a maybe
decodeResultToMaybe :: DecodeResult a -> Maybe a
decodeResultToMaybe (DecodeOk a) = Just a
decodeResultToMaybe _            = Nothing

-- | Document model.  This does not need to be able to completely represent all
-- the data in the XML document.  In fact, having a smaller model may improve
-- query performance.
data Plant = Plant
  { common :: Text
  , price  :: Text
  } deriving (Eq, Show)

newtype Catalog = Catalog
  { plants :: [Plant]
  } deriving (Eq, Show)

-- | Decode plant element
decodePlant :: Value -> DecodeResult Plant
decodePlant xml = do
  aCommon <- xml /> "common"  >>= parseText
  aPrice  <- xml /> "price"   >>= parseText
  return $ Plant aCommon aPrice

-- | Decode catalog element
decodeCatalog :: Value -> DecodeResult Catalog
decodeCatalog xml = do
  aPlantXmls <- xml />> "plant"
  let aPlants = catMaybes (decodeResultToMaybe . decodePlant <$> aPlantXmls)
  return $ Catalog aPlants

runDemo :: Z.DemoOptions -> IO ()
runDemo _ = do
  -- Read XML into memory as a query-optimised cursor
  !cursor <- loadFastCursor "components/succinct-xml/data/catalog.xml"
  -- Skip the XML declaration to get to the root element cursor
  case nextSibling cursor of
    Just rootCursor -> do
      -- Get the root raw XML value at the root element cursor
      let rootValue = rawValueAt (xmlIndexAt rootCursor)
      -- Show what we have at this cursor
      putStrLn $ "Raw value: " <> take 100 (show rootValue)
      -- Decode the raw XML value
      case decodeCatalog (rawDecode rootValue) of
        DecodeOk catalog -> putStrLn $ "Catalog: " <> show catalog
        DecodeFailed msg -> putStrLn $ "Error: " <> show msg
    Nothing -> do
      putStrLn "Could not read XML"
      return ()

optsDemo :: Parser Z.DemoOptions
optsDemo = pure Z.DemoOptions

cmdDemo :: Mod CommandFields (IO ())
cmdDemo = command "demo"  $ flip info idm $ runDemo <$> optsDemo
