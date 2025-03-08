{-# LANGUAGE BangPatterns        #-}

module Bench.RankSelect
  ( mkBenchRankSelect
  , runCsPoppyBuild
  ) where

import Control.Monad
import Criterion.Main
import Data.List
import Data.Succinct.RankSelect.Base.Rank1
import Data.Succinct.RankSelect.Base.Select1
import Data.Succinct.RankSelect.CsPoppy
import Data.Succinct.RankSelect.CsPoppy.Internal.Alpha1
import Data.Succinct.RankSelect.CsPoppy.Internal.CsInterleaved
import Data.Succinct.RankSelect.CsPoppy.Internal.Reference
import Data.Succinct.RankSelect.CsPoppy.Internal.Vector
import Data.Succinct.RankSelect.Poppy512
import Data.Word
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import System.Directory

import qualified Data.Succinct.RankSelect.CsPoppy  as CS
import qualified Data.Succinct.RankSelect.Poppy512 as P512
import qualified Data.Vector.Storable              as DVS

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

benchCsPoppyBuild :: IO [Benchmark]
benchCsPoppyBuild = do
  entries <- getDirectoryContents "components/succinct-core/data"
  let files = ("components/succinct-core/data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (mmapFromForeignRegion filename) $ \bitString -> bgroup filename
          [ bench "CsPoppy Build"  (whnf CS.makeCsPoppy bitString)
          ]

benchCsPoppyRank1 :: IO [Benchmark]
benchCsPoppyRank1 = do
  entries <- getDirectoryContents "components/succinct-core/data"
  let files = ("components/succinct-core/data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (mmapFromForeignRegion filename) $ \(v :: CsPoppy) -> bgroup filename
          [ bench "CsPoppy Rank1"  (whnf (rank1 v) 100)
          ]

benchCsPoppySelect1 :: IO [Benchmark]
benchCsPoppySelect1 = do
  entries <- getDirectoryContents "components/succinct-core/data"
  let files = ("components/succinct-core/data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (mmapFromForeignRegion filename) $ \(rsbs :: CsPoppy) -> bgroup filename
          [ bench "CsPoppy Select1"  (whnf (go rsbs 1 (popCount1 rsbs) ((popCount1 rsbs `div` 100) + 1)) 0)
          ]
        go rsbs a z step acc | a <= z  = go rsbs (a + step) z step (select1 rsbs a + acc)
        go _    _ _ _    acc = acc
        {-# INLINE go #-}

benchPoppy512Build :: IO [Benchmark]
benchPoppy512Build = do
  entries <- getDirectoryContents "components/succinct-core/data"
  let files = ("components/succinct-core/data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (mmapFromForeignRegion filename) $ \bitString -> bgroup filename
          [ bench "Poppy512 Build"  (whnf P512.makePoppy512 bitString)
          ]

benchPoppy512Rank1 :: IO [Benchmark]
benchPoppy512Rank1 = do
  entries <- getDirectoryContents "components/succinct-core/data"
  let files = ("components/succinct-core/data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (mmapFromForeignRegion filename) $ \(rsbs :: Poppy512) -> bgroup filename
          [ bench "Poppy512 Rank1"  (whnf (rank1 rsbs) 100)
          ]

benchPoppy512Select1 :: IO [Benchmark]
benchPoppy512Select1 = do
  entries <- getDirectoryContents "components/succinct-core/data"
  let files = ("components/succinct-core/data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (mmapFromForeignRegion filename) $ \(rsbs :: Poppy512) -> bgroup filename
          [ bench "Poppy512 Select1"  (whnf (go rsbs 1 (popCount1 rsbs) ((popCount1 rsbs `div` 100) + 1)) 0)
          ]
        go rsbs a z step acc | a <= z  = go rsbs (a + step) z step (select1 rsbs a + acc)
        go _    _ _ _    acc = acc
        {-# INLINE go #-}

runCsPoppyBuild :: IO ()
runCsPoppyBuild = do
  entries <- getDirectoryContents "components/succinct-core/data"
  let files = ("components/succinct-core/data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  forM_ files $ \file -> do
    msbs :: CsPoppy <- mmapFromForeignRegion file
    let !_ = select1 msbs 1
    return ()

benchMakeCsPoppyBlocks :: IO [Benchmark]
benchMakeCsPoppyBlocks = do
  entries <- getDirectoryContents "components/succinct-core/data"
  let files = ("components/succinct-core/data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (mmapFromForeignRegion filename) $ \(v :: DVS.Vector Word64) -> bgroup filename
          [ bench "makeCsPoppyBlocks"  (whnf makeCsPoppyBlocks v)
          ]

benchMakeCsPoppyLayerM :: IO [Benchmark]
benchMakeCsPoppyLayerM = do
  entries <- getDirectoryContents "components/succinct-core/data"
  let files = ("components/succinct-core/data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (mkEnv filename) $ \(v :: DVS.Vector Word64) -> bgroup filename
          [ bench "makeCsPoppyLayerM"  (whnf makeCsPoppyLayerM  v)
          , bench "makeCsPoppyLayerM2" (whnf makeCsPoppyLayerM2 v)
          ]
        mkEnv filename = do
          !v <- mmapFromForeignRegion filename
          return (makeCsPoppyBlocks v)

benchGenCsSamples :: IO [Benchmark]
benchGenCsSamples = do
  entries <- getDirectoryContents "components/succinct-core/data"
  let files = ("components/succinct-core/data/" ++) <$> (".ib" `isSuffixOf`) `filter` entries
  return (mkBenchmark <$> files)
  where mkBenchmark filename = env (mkEnv filename) $ \ ~(pc, v :: DVS.Vector Word64) -> bgroup filename
          [ bench "benchGenCsSamples"  (whnf (genCsSamples pc) v)
          ]
        mkEnv filename = do
          !v <- mmapFromForeignRegion filename
          let !blocks          = makeCsPoppyBlocks v
          let !layerM          = makeCsPoppyLayerM blocks
          return (getCsiTotal (CsInterleaved (lastOrZero layerM)), v)

mkBenchRankSelect :: IO [Benchmark]
mkBenchRankSelect = do
  benchmarks <- (concat <$>) $ sequence $ []
    <> [benchCsPoppyBuild]
    <> [benchCsPoppyRank1]
    <> [benchCsPoppySelect1]
    <> [benchPoppy512Build]
    <> [benchPoppy512Rank1]
    <> [benchPoppy512Select1]
    <> [benchMakeCsPoppyBlocks]
    <> [benchMakeCsPoppyLayerM]
    <> [benchGenCsSamples]
  when (null benchmarks) $ putStrLn "Warning: No benchmarks found"
  pure benchmarks
