




{-# LANGUAGE OverloadedStrings     #-}



module Data.Succinct.Json.Standard.Cursor.Generic
  ( GenericCursor(..)
  , jsonCursorPos
  ) where

import Data.Succinct.RankSelect.Base.Rank0
import Data.Succinct.RankSelect.Base.Rank1
import Data.Succinct.RankSelect.Base.Select1
import GHC.Generics
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.TreeCursor
import Prelude                                   hiding (drop)

import qualified Data.Succinct.BalancedParens as BP

data GenericCursor t v w = GenericCursor
  { cursorText     :: !t
  , interests      :: !v
  , balancedParens :: !w
  , cursorRank     :: !Count
  }
  deriving (Eq, Generic, Show)

instance (BP.BalancedParens u, Rank1 u, Rank0 u) => TreeCursor (GenericCursor t v u) where
  firstChild :: GenericCursor t v u -> Maybe (GenericCursor t v u)
  firstChild k = let mq = BP.firstChild (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  nextSibling :: GenericCursor t v u -> Maybe (GenericCursor t v u)
  nextSibling k = (\q -> k { cursorRank = q }) <$> BP.nextSibling (balancedParens k) (cursorRank k)

  parent :: GenericCursor t v u -> Maybe (GenericCursor t v u)
  parent k = let mq = BP.parent (balancedParens k) (cursorRank k) in (\q -> k { cursorRank = q }) <$> mq

  depth :: GenericCursor t v u -> Maybe Count
  depth k = BP.depth (balancedParens k) (cursorRank k)

  subtreeSize :: GenericCursor t v u -> Maybe Count
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

jsonCursorPos :: (Rank1 w, Select1 v) => GenericCursor s v w -> Position
jsonCursorPos k = toPosition (select1 ik (rank1 bpk (cursorRank k)) - 1)
  where ik  = interests k
        bpk = balancedParens k
