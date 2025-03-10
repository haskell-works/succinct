{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Succinct.Xml.Succinct.Index
( XmlIndex(..)
, XmlIndexAt(..)
)
where

import Control.Arrow
import Data.Succinct.RankSelect.Base.Rank0
import Data.Succinct.RankSelect.Base.Rank1
import Data.Succinct.RankSelect.Base.Select1
import Data.Succinct.Xml.CharLike
import Data.Succinct.Xml.Grammar
import Data.Succinct.Xml.Succinct
import Data.Text                                 (Text)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.TreeCursor
import HaskellWorks.Data.Uncons
import Prelude                                   hiding (drop)

import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString                  as BS
import qualified Data.List                        as L
import qualified Data.Succinct.BalancedParens     as BP
import qualified Data.Text                        as T

data XmlIndex
  = XmlIndexDocument [XmlIndex]
  | XmlIndexElement Text [XmlIndex]
  | XmlIndexCData BS.ByteString
  | XmlIndexComment BS.ByteString
  | XmlIndexMeta Text [XmlIndex]
  | XmlIndexAttrList [XmlIndex]
  | XmlIndexValue BS.ByteString
  | XmlIndexAttrName BS.ByteString
  | XmlIndexAttrValue BS.ByteString
  | XmlIndexError Text
  deriving (Eq, Show)

data XmlIndexState
  = InAttrList
  | InElement
  | Unknown
  deriving (Eq, Show)

class XmlIndexAt a where
  xmlIndexAt :: a -> XmlIndex

pos :: (Select1 v, Rank1 w) => XmlCursor t v w -> Position
pos c = lastPositionOf (select1 (interests c) (rank1 (balancedParens c) (cursorRank c)))

remText :: (Drop v, Select1 v1, Rank1 w) => XmlCursor v v1 w -> v
remText c = drop (toCount (pos c)) (cursorText c)

instance (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => XmlIndexAt (XmlCursor BS.ByteString v w) where
  xmlIndexAt :: XmlCursor BS.ByteString v w -> XmlIndex
  xmlIndexAt = getIndexAt Unknown


getIndexAt :: (BP.BalancedParens w, Rank0 w, Rank1 w, Select1 v, TestBit w) => XmlIndexState -> XmlCursor BS.ByteString v w -> XmlIndex
getIndexAt state k = case uncons remainder of
  Just (!c, cs) | isElementStart c         -> parseElem cs
  Just (!c, _ ) | isSpace c                -> XmlIndexAttrList $ mapValuesFrom InAttrList (firstChild k)
  Just (!c, _ ) | isAttribute && isQuote c -> XmlIndexAttrValue remainder
  Just _        | isAttribute              -> XmlIndexAttrName remainder
  Just _                                   -> XmlIndexValue remainder
  Nothing                                  -> XmlIndexError "End of data"
  where remainder         = remText k
        mapValuesFrom s   = L.unfoldr (fmap (getIndexAt s &&& nextSibling))
        isAttribute = case state of
          InAttrList -> True
          InElement  -> False
          Unknown    -> case remText <$> parent k >>= uncons of
            Just (!c, _) | isSpace c -> True
            _                        -> False

        parseElem bs =
          case ABC.parse parseXmlElement bs of
            ABC.Fail {}    -> decodeErr "Unable to parse element name" bs
            ABC.Partial _  -> decodeErr "Unexpected end of string" bs
            ABC.Done i r   -> case r of
              XmlElementTypeCData     -> XmlIndexCData i
              XmlElementTypeComment   -> XmlIndexComment i
              XmlElementTypeMeta s    -> XmlIndexMeta s    (mapValuesFrom InElement $ firstChild k)
              XmlElementTypeElement s -> XmlIndexElement s (mapValuesFrom InElement $ firstChild k)
              XmlElementTypeDocument  -> XmlIndexDocument  (mapValuesFrom InElement (firstChild k) <> mapValuesFrom InElement (nextSibling k))

decodeErr :: String -> BS.ByteString -> XmlIndex
decodeErr reason bs = XmlIndexError . T.pack $ reason <>": " <> show (BS.take 20 bs) <> "...'"
