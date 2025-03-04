




{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}


{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Succinct.Json.Standard.Cursor.TypeSpec (spec) where

import Control.Monad
import Data.Succinct.BalancedParens.BalancedParens
import Data.Succinct.Json.Standard.Cursor.Generic
import Data.Succinct.Json.Standard.Cursor.Type
import Data.Succinct.RankSelect.Base.Rank0
import Data.Succinct.RankSelect.Base.Rank1
import Data.Succinct.RankSelect.Base.Select1
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                         as BS
import qualified Data.Succinct.Json.Standard.Cursor.Fast as FAST
import qualified Data.Succinct.Json.Standard.Cursor.Slow as SLOW
import qualified HaskellWorks.Data.TreeCursor            as TC

{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant do"        -}

fc = TC.firstChild
ns = TC.nextSibling

spec :: Spec
spec = describe "Data.Succinct.Json.Succinct.CursorSpec" $ do
  genSpec "DVS.Vector Word64" SLOW.fromString
  genSpec "CsPoppy1"          FAST.fromString

genSpec :: forall t u.
  ( Eq                t
  , Show              t
  , Select1           t
  , Eq                u
  , Show              u
  , Rank0             u
  , Rank1             u
  , BalancedParens    u
  , TestBit           u)
  => String -> (String -> GenericCursor BS.ByteString t u) -> SpecWith ()
genSpec t makeCursor = do
  describe ("Json cursor of type " ++ t) $ do
    let forJson s f = describe ("of value " ++ show s) (f (makeCursor s))
    forJson "{}" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeObject
    forJson " {}" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeObject
    forJson "1234" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeNumber
    forJson "\"Hello\"" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeString
    forJson "[]" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeArray
    forJson "true" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeBool
    forJson "false" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeBool
    forJson "null" $ \cursor -> do
      it "should have correct type"       $ requireTest $         jsonTypeAt  cursor === Just JsonTypeNull
    forJson "[null]" $ \cursor -> do
      it "should have correct type"       $ requireTest $ (fc >=> jsonTypeAt) cursor === Just JsonTypeNull
    forJson "[null, {\"field\": 1}]" $ \cursor -> do
      it "cursor can navigate to second child of array" $ requireTest $ do
        (fc >=> ns >=> jsonTypeAt) cursor === Just JsonTypeObject
      it "cursor can navigate to first child of object at second child of array" $ requireTest $ do
        (fc >=> ns >=> fc >=> jsonTypeAt) cursor === Just JsonTypeString
      it "cursor can navigate to first child of object at second child of array" $ requireTest $ do
        (fc >=> ns >=> fc >=> ns >=> jsonTypeAt) cursor === Just JsonTypeNumber
    describe "For empty json array" $ do
      let cursor = makeCursor "[null]"
      it "can navigate down and forwards" $ requireTest $ do
        (                     jsonTypeAt) cursor === Just JsonTypeArray
        (fc               >=> jsonTypeAt) cursor === Just JsonTypeNull
        (fc >=> ns        >=> jsonTypeAt) cursor === Nothing
        (fc >=> ns >=> ns >=> jsonTypeAt) cursor === Nothing
    describe "For sample Json" $ do
      let cursor = makeCursor "{ \
                    \    \"widget\": { \
                    \        \"debug\": \"on\", \
                    \        \"window\": { \
                    \            \"name\": \"main_window\", \
                    \            \"dimensions\": [500, 600.01e-02, true, false, null] \
                    \        } \
                    \    } \
                    \}" :: GenericCursor BS.ByteString t u
      it "can navigate down and forwards" $ requireTest $ do
        (                                                                      jsonTypeAt) cursor === Just JsonTypeObject
        (fc                                                                >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns                                                         >=> jsonTypeAt) cursor === Just JsonTypeObject
        (fc >=> ns >=> fc                                                  >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns                                           >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns                                    >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns                             >=> jsonTypeAt) cursor === Just JsonTypeObject
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                      >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns               >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns        >=> jsonTypeAt) cursor === Just JsonTypeString
        (fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> jsonTypeAt) cursor === Just JsonTypeArray
