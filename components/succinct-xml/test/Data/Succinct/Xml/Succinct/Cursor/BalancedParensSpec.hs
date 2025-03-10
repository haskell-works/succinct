{-# LANGUAGE OverloadedStrings #-}

module Data.Succinct.Xml.Succinct.Cursor.BalancedParensSpec
  ( spec
  ) where

import Data.String
import Data.Succinct.Xml.Internal.BalancedParens
import Data.Succinct.Xml.Internal.Blank
import Data.Succinct.Xml.Internal.List
import Data.Succinct.Xml.Succinct.Cursor.BlankedXml
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.ByteString
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString as BS

{- HLINT ignore "Redundant do"        -}

spec :: Spec
spec = describe "Data.Succinct.Xml.Succinct.Cursor.BalancedParensSpec" $ do
  it "Blanking XML should work 1" $ requireTest $ do
    let blankedXml = BlankedXml ["<t<t>>"]
    let bp = BitShown $ BS.concat (compressWordAsBit (blankedXmlToBalancedParens (getBlankedXml blankedXml)))
    bp === fromString "11011000"
  it "Blanking XML should work 2" $ requireTest $ do
    let blankedXml = BlankedXml
          [ "<><><><><><><><>"
          , "<><><><><><><><>"
          ]
    let bp = BitShown $ BS.concat (compressWordAsBit (blankedXmlToBalancedParens (getBlankedXml blankedXml)))
    bp === fromString
          "1010101010101010\
          \1010101010101010"

  let unchunkedInput = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<micro_stats>\n  <metric>\n  </metric>\n  <metric></metric>\n  <metric></metric>\n  <metric></metric>\n  <metric></metric>\n</micro_stats>\n"
  let chunkedInput = chunkedBy 15 unchunkedInput
  let chunkedBlank = blankXml chunkedInput

  let unchunkedBadInput = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<micro_stats>\n  <metric>       \n  </metric>\n  <metric></metric>\n  <metric></metric>\n  <metric></metric>\n  <metric></metric>\n</micro_stats>\n"
  let chunkedBadInput = chunkedBy 15 unchunkedBadInput
  let chunkedBadBlank = blankXml chunkedBadInput

  it "Same input" $ requireTest $ do
    unchunkedInput === BS.concat chunkedInput

  it "Blanking XML should work 3" $ requireTest $ do
    let bp = BitShown $ BS.concat (compressWordAsBit (blankedXmlToBalancedParens chunkedBlank))
    annotate $ "Good: " <> show chunkedBlank
    bp === fromString "11101010 10001101 01010100"

  it "Blanking XML should work 3" $ requireTest $ do
    let bp = BitShown $ BS.concat (compressWordAsBit (blankedXmlToBalancedParens chunkedBadBlank))
    annotate $ "Bad: " <> show chunkedBadBlank
    bp === fromString "11101010 10001101 01010100"

  describe "Chunking works" $ do
    let document = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><a text='value'>free</a>"
    let whole = mkBlank 4096 document
    let chunked = mkBlank 15 document

    it "should BP the same with chanks" $ requireTest $ do
      BS.concat chunked === BS.concat whole

    it "should produce same bits" $ requireTest $ do
      BS.concat (mkBits chunked) === BS.concat (mkBits whole)


mkBlank :: Int -> BS.ByteString -> [BS.ByteString]
mkBlank csize bs = blankXml (chunkedBy csize bs)

mkBits :: [BS.ByteString] -> [BS.ByteString]
mkBits = compressWordAsBit . blankedXmlToBalancedParens
