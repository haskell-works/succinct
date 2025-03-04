

module Data.Succinct.Json.Standard.Cursor.Specific
  ( SpecificCursor(..)
  , jsonCursorPos
  ) where

import Data.Succinct.Json.Standard.Cursor.Generic

class SpecificCursor w where
  type CursorOf w
