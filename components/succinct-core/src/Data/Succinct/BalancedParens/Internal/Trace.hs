module Data.Succinct.BalancedParens.Internal.Trace
  ( traceW
  ) where

import Debug.Trace

traceW :: Show a => String -> a -> a
traceW s a = trace (s <> " = " <> show a) a
