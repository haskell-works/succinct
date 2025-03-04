



{-# LANGUAGE OverloadedStrings     #-}


module Data.Succinct.Xml.Grammar where

import Control.Applicative
import Data.Char
import Data.String
import Data.Succinct.Parser
import Data.Text                (Text)
import Data.Word

import qualified Data.Attoparsec.Types as T
import qualified Data.Succinct.Parser  as P
import qualified Data.Text             as T

data XmlElementType
  = XmlElementTypeDocument
  | XmlElementTypeElement Text
  | XmlElementTypeComment
  | XmlElementTypeCData
  | XmlElementTypeMeta Text

parseXmlString :: (P.Parser t Word8) => T.Parser t Text
parseXmlString = do
  q <- satisfyChar (=='"') <|> satisfyChar (=='\'')
  T.pack <$> many (satisfyChar (/= q))

parseXmlElement :: (P.Parser t Word8, IsString t) => T.Parser t XmlElementType
parseXmlElement = comment <|> cdata <|> doc <|> meta <|> element
  where
  comment = const XmlElementTypeComment   <$> string "!--"
  cdata   = const XmlElementTypeCData     <$> string "![CDATA["
  meta    = XmlElementTypeMeta            <$> (string "!" >> parseXmlToken)
  doc     = const XmlElementTypeDocument  <$> string "?xml"
  element = XmlElementTypeElement         <$> parseXmlToken

parseXmlToken :: (P.Parser t Word8) => T.Parser t Text
parseXmlToken = T.pack <$> many (satisfyChar isNameChar <?> "invalid string character")

parseXmlAttributeName :: (P.Parser t Word8) => T.Parser t Text
parseXmlAttributeName = parseXmlToken

isNameStartChar :: Char -> Bool
isNameStartChar w =
  let iw = ord w
  in w == '_' || w == ':' || isAlpha w
     || (iw >= 0xc0 && iw <= 0xd6)
     || (iw >= 0xd8 && iw <= 0xf6)
     || (iw >= 0xf8 && iw <= 0xff)

isNameChar :: Char -> Bool
isNameChar w = isNameStartChar w || w == '-' || w == '.'
             || ord w == 0xb7 || isNumber w
