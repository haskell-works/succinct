{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bench.BalancedParens
import Bench.Excess
import Bench.RankSelect
import Bench.RankSelectBase
import Criterion.Main
import Data.Bits.Pdep

main :: IO ()
main = do
  putStrLn $ "Fast pdep enabled: " <> show fastPdepEnabled
  benchExcess <- mconcat <$> sequence
    [ makeBenchExcess
    ]
  benchBalancedParens <- mkBenchBalancedParens
  benchRankSelect <- mkBenchRankSelect

  defaultMain $ mconcat
    [ benchRankSelect
    , benchRankSelectBase
    , benchExcess
    , benchBalancedParens
    ]
