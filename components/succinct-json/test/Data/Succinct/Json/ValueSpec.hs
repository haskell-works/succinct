


{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}


{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Succinct.Json.ValueSpec (spec) where

import Control.Monad
import Data.Succinct.BalancedParens.BalancedParens
import HaskellWorks.Data.Bits.BitWise
import Data.Succinct.Json.DecodeError
import Data.Succinct.Json.Internal.Index
import Data.Succinct.Json.Standard.Cursor.Generic
import Data.Succinct.Json.Value
import Data.Succinct.RankSelect.Base.Rank0
import Data.Succinct.RankSelect.Base.Rank1
import Data.Succinct.RankSelect.Base.Select1
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                         as BS
import qualified Data.Succinct.Json.Standard.Cursor.Fast as FAST
import qualified Data.Succinct.Json.Standard.Cursor.Slow as SLOW
import qualified HaskellWorks.Data.TreeCursor            as TC

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant bracket"  -}
{- HLINT ignore "Redundant do"       -}

fc = TC.firstChild
ns = TC.nextSibling

spec :: Spec
spec = describe "Data.Succinct.Json.ValueSpec" $ do
  genSpec "DVS.Vector Word64" SLOW.fromString
  genSpec "CsPoppy"           FAST.fromString

jsonValueVia  ::
  ( BalancedParens u
  , Rank0 u
  , Rank1 u
  , Select1 t
  , TestBit u)
  => Maybe (GenericCursor BS.ByteString t u) -> Either DecodeError JsonValue
jsonValueVia mk = case mk of
  Just k  -> (jsonIndexAt >=> jsonValueAt) k
  Nothing -> Left (DecodeError "No such element")

genSpec :: forall t u.
  ( Select1           t
  , Rank0             u
  , Rank1             u
  , BalancedParens    u
  , TestBit           u)
  => String -> (String -> GenericCursor BS.ByteString t u) -> SpecWith ()
genSpec t makeCursor = do
  describe ("Json cursor of type " ++ t) $ do
    let forJson s f = describe ("of value " ++ show s) (f (makeCursor s))
    forJson "{}" $ \cursor -> do
      it "should have correct value"      $ requireTest $ jsonValueVia (Just cursor) === Right (JsonObject [])
    forJson " {}" $ \cursor -> do
      it "should have correct value"      $ requireTest $ jsonValueVia (Just cursor) === Right (JsonObject [])
    forJson "1234" $ \cursor -> do
      it "should have correct value"      $ requireTest $ jsonValueVia (Just cursor) === Right (JsonNumber 1234)
    forJson "\"Hello\"" $ \cursor -> do
      it "should have correct value"      $ requireTest $ jsonValueVia (Just cursor) === Right (JsonString "Hello")
    forJson "[]" $ \cursor -> do
      it "should have correct value"      $ requireTest $ jsonValueVia (Just cursor) === Right (JsonArray [])
    forJson "true" $ \cursor -> do
      it "should have correct value"      $ requireTest $ jsonValueVia (Just cursor) === Right (JsonBool True)
    forJson "false" $ \cursor -> do
      it "should have correct value"      $ requireTest $ jsonValueVia (Just cursor) === Right (JsonBool False)
    forJson "null" $ \cursor -> do
      it "should have correct value"      $ requireTest $ jsonValueVia (Just cursor) === Right JsonNull
    forJson "[null]" $ \cursor -> do
      it "should have correct value"      $ requireTest $ jsonValueVia (Just cursor) === Right (JsonArray [JsonNull])
      it "should have correct value"      $ requireTest $ jsonValueVia (fc   cursor) === Right  JsonNull
      it "should have correct value"      $ requireTest $ jsonValueVia (fc   cursor) === Right  JsonNull
    forJson "\"codepoint\\u2019s\"" $ \cursor -> do
      it "should have correct value"      $ requireTest $ jsonValueVia (Just cursor) === Right (JsonString "codepoint’s")
    forJson "[null, {\"field\": 1}]" $ \cursor -> do
      it "cursor can navigate to second child of array" $ requireTest $ do
        jsonValueVia ((fc >=> ns)   cursor) === Right (                     JsonObject [("field", JsonNumber 1)] )
        jsonValueVia (Just          cursor) === Right (JsonArray [JsonNull, JsonObject [("field", JsonNumber 1)]])
    describe "For empty json array" $ do
      let cursor = makeCursor "[]"
      it "can navigate down and forwards" $ requireTest $ do
        jsonValueVia (Just cursor) === Right (JsonArray [])
    describe "For empty json array" $ do
      let cursor = makeCursor "[null]"
      it "can navigate down and forwards" $ requireTest $ do
        jsonValueVia (Just cursor) === Right (JsonArray [JsonNull])
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
        let array   = JsonArray [JsonNumber 500, JsonNumber 600.01e-02, JsonBool True, JsonBool False, JsonNull] :: JsonValue
        let object1 = JsonObject ([("name", JsonString "main_window"), ("dimensions", array)]) :: JsonValue
        let object2 = JsonObject ([("debug", JsonString "on"), ("window", object1)]) :: JsonValue
        let object3 = JsonObject ([("widget", object2)]) :: JsonValue
        jsonValueVia (Just                                                                                                   cursor) === Right object3
        jsonValueVia ((fc                                                                                                  ) cursor) === Right (JsonString "widget"      )
        jsonValueVia ((fc >=> ns                                                                                           ) cursor) === Right (object2                  )
        jsonValueVia ((fc >=> ns >=> fc                                                                                    ) cursor) === Right (JsonString "debug"       )
        jsonValueVia ((fc >=> ns >=> fc >=> ns                                                                             ) cursor) === Right (JsonString "on"          )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns                                                                      ) cursor) === Right (JsonString "window"      )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns                                                               ) cursor) === Right (object1                  )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                                                        ) cursor) === Right (JsonString "name"        )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                                                 ) cursor) === Right (JsonString "main_window" )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns                                          ) cursor) === Right (JsonString "dimensions"  )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns                                   ) cursor) === Right (array                    )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc                            ) cursor) === Right (JsonNumber 500           )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns                     ) cursor) === Right (JsonNumber 600.01e-02    )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns              ) cursor) === Right (JsonBool True            )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns       ) cursor) === Right (JsonBool False           )
        jsonValueVia ((fc >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> fc >=> ns >=> ns >=> ns >=> ns) cursor) === Right JsonNull
