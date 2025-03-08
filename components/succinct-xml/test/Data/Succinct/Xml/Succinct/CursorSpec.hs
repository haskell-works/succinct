{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Succinct.Xml.Succinct.CursorSpec(spec) where

import Data.Succinct.BalancedParens.Simple
import Data.Succinct.RankSelect.Poppy512
import Data.Succinct.Xml.Succinct.Cursor           as C
import Data.Succinct.Xml.Succinct.CursorSpec.Make
import Data.Word
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                          as BS
import qualified Data.Vector.Storable                     as DVS
import qualified HaskellWorks.Data.FromByteString         as BS
import qualified Data.Succinct.Xml.Succinct.Cursor.Create as CC

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant bracket"   -}
{- HLINT ignore "Reduce duplication"  -}

spec :: Spec
spec = describe "Data.Succinct.Xml.Succinct.CursorSpec" $ do
  make "DVS.Vector Word8"  (BS.fromByteString :: BS.ByteString -> XmlCursor BS.ByteString (BitShown (DVS.Vector Word8 )) (SimpleBalancedParens (DVS.Vector Word8 )))
  make "DVS.Vector Word16" (BS.fromByteString :: BS.ByteString -> XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16)))
  make "DVS.Vector Word32" (BS.fromByteString :: BS.ByteString -> XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32)))
  make "DVS.Vector Word64" (BS.fromByteString :: BS.ByteString -> XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  make "Poppy512"          (BS.fromByteString :: BS.ByteString -> XmlCursor BS.ByteString  Poppy512                      (SimpleBalancedParens (DVS.Vector Word64)))
  make "DVS.Vector Word8"  CC.byteStringAsFastCursor
  make "DVS.Vector Word16" CC.byteStringAsFastCursor
  make "DVS.Vector Word32" CC.byteStringAsFastCursor
  make "DVS.Vector Word64" CC.byteStringAsFastCursor
  make "Poppy512"          CC.byteStringAsFastCursor
  it "Loads same Xml consistentally from different backing vectors" $ requireTest $ do
    let cursor8   = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))
    let cursor16  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))
    let cursor32  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))
    let cursor64  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: XmlCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
    cursorText cursor8 === cursorText cursor16
    cursorText cursor8 === cursorText cursor32
    cursorText cursor8 === cursorText cursor64
    let ic8   = bitShow $ interests cursor8
    let ic16  = bitShow $ interests cursor16
    let ic32  = bitShow $ interests cursor32
    let ic64  = bitShow $ interests cursor64
    ic16 `shouldBeginWith` ic8
    ic32 `shouldBeginWith` ic16
    ic64 `shouldBeginWith` ic32

shouldBeginWith :: (Eq a, Show a) => [a] -> [a] -> PropertyT IO ()
shouldBeginWith as bs = take (length bs) as === bs
