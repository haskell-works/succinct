{-# LANGUAGE ExplicitForAll            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module HaskellWorks.Data.Json.Simple.CursorSpec
  ( spec
  ) where

import Control.Monad
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Json.Simple.Cursor         as Z
import qualified HaskellWorks.Data.Json.Simple.Cursor.Fast    as FAST
import qualified HaskellWorks.Data.Json.Simple.Cursor.Snippet as S
import qualified HaskellWorks.Data.TreeCursor                 as TC

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant bracket"   -}
{- HLINT ignore "Reduce duplication"  -}

fc = TC.firstChild
ns = TC.nextSibling

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Backend.Simple.CursorSpec" $ do
  describe "Json cursor" $ do
    describe "For sample Json" $ do
      let k = FAST.fromByteString "[[11],[22]]"
      -- [  [  1  1 ]  ,  [  2  2 ]  ]
      -- (( ((      )) )( ((      )) ))
      it "can navigate" $ requireTest $ do
        (Z.cursorRank <$>  Just                            k) === Just 1
        (Z.cursorRank <$>  ns                              k) === Nothing
        (Z.cursorRank <$>  fc                              k) === Just 2
        (Z.cursorRank <$> (fc >=> ns                     ) k) === Just 8
        (Z.cursorRank <$> (fc >=> ns >=> fc              ) k) === Just 9
        (Z.cursorRank <$> (fc >=> ns >=> fc >=> ns       ) k) === Nothing
        (Z.cursorRank <$> (fc >=> ns >=> fc >=> fc       ) k) === Just 10
        (Z.cursorRank <$> (fc >=> ns >=> fc >=> fc >=> ns) k) === Nothing
        (Z.cursorRank <$> (fc >=> ns >=> fc >=> fc >=> fc) k) === Nothing
        (Z.cursorRank <$> (fc >=> fc                     ) k) === Just 3
        (Z.cursorRank <$> (fc >=> fc >=> ns              ) k) === Nothing
        (Z.cursorRank <$> (fc >=> fc >=> fc              ) k) === Just 4
        (Z.cursorRank <$> (fc >=> fc >=> fc >=> ns       ) k) === Nothing
        (Z.cursorRank <$> (fc >=> fc >=> fc >=> fc       ) k) === Nothing
      it "can snippet pos" $ requireTest $ do
        (S.snippetPos <$>  Just                            k) === Just (1, 11)
        (S.snippetPos <$>  ns                              k) === Nothing
        (S.snippetPos <$>  fc                              k) === Just (2,  5)
        (S.snippetPos <$> (fc >=> ns                     ) k) === Just (7, 10)
        (S.snippetPos <$> (fc >=> ns >=> fc              ) k) === Just (7, 10)
        (S.snippetPos <$> (fc >=> ns >=> fc >=> ns       ) k) === Nothing
        (S.snippetPos <$> (fc >=> ns >=> fc >=> fc       ) k) === Just (8,  9)
        (S.snippetPos <$> (fc >=> ns >=> fc >=> fc >=> ns) k) === Nothing
        (S.snippetPos <$> (fc >=> ns >=> fc >=> fc >=> fc) k) === Nothing
        (S.snippetPos <$> (fc >=> fc                     ) k) === Just (2,  5)
        (S.snippetPos <$> (fc >=> fc >=> ns              ) k) === Nothing
        (S.snippetPos <$> (fc >=> fc >=> fc              ) k) === Just (3,  4)
        (S.snippetPos <$> (fc >=> fc >=> fc >=> ns       ) k) === Nothing
        (S.snippetPos <$> (fc >=> fc >=> fc >=> fc       ) k) === Nothing
      it "can snippet" $ requireTest $ do
        (S.snippet <$>  Just                            k) === Just "[[11],[22]]"
        (S.snippet <$>  ns                              k) === Nothing
        (S.snippet <$>  fc                              k) === Just "[11]"
        (S.snippet <$> (fc >=> ns                     ) k) === Just "[22]"
        (S.snippet <$> (fc >=> ns >=> fc              ) k) === Just "[22]"
        (S.snippet <$> (fc >=> ns >=> fc >=> ns       ) k) === Nothing
        (S.snippet <$> (fc >=> ns >=> fc >=> fc       ) k) === Just "22"
        (S.snippet <$> (fc >=> ns >=> fc >=> fc >=> ns) k) === Nothing
        (S.snippet <$> (fc >=> ns >=> fc >=> fc >=> fc) k) === Nothing
        (S.snippet <$> (fc >=> fc                     ) k) === Just "[11]"
        (S.snippet <$> (fc >=> fc >=> ns              ) k) === Nothing
        (S.snippet <$> (fc >=> fc >=> fc              ) k) === Just "11"
        (S.snippet <$> (fc >=> fc >=> fc >=> ns       ) k) === Nothing
        (S.snippet <$> (fc >=> fc >=> fc >=> fc       ) k) === Nothing
