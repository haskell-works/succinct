{-# LANGUAGE BangPatterns        #-}


module Main where

import App.Commands
import Control.Monad
import Data.Succinct.RankSelect.Base.Select1
import Data.Succinct.RankSelect.CsPoppy
import Data.Succinct.RankSelect.Poppy512
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromForeignRegion
import Options.Applicative
import System.Directory

import qualified Data.List as L

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

runPoppy512SelectAll :: IO ()
runPoppy512SelectAll = do
  entries <- getDirectoryContents "components/succinct-dsv/data"
  let files = ("components/succinct-dsv/data/" ++) <$> (".ib" `L.isSuffixOf`) `filter` entries
  forM_ files $ \file -> do
    putStrLn $ "Loading cspoppy for " <> file
    v :: Poppy512 <- mmapFromForeignRegion file
    forM_ [1..popCount1 v] $ \i -> do
      let !_ = select1 v i
      return ()
    return ()

runCsPoppySelectAll :: IO ()
runCsPoppySelectAll = do
  entries <- getDirectoryContents "components/succinct-dsv/data"
  let files = ("components/succinct-dsv/data/" ++) <$> (".ib" `L.isSuffixOf`) `filter` entries
  forM_ files $ \file -> do
    putStrLn $ "Loading cspoppy for " <> file
    v :: CsPoppy <- mmapFromForeignRegion file
    forM_ [1..popCount1 v] $ \i -> do
      let !_ = select1 v i
      return ()
    return ()

main :: IO ()
main = join $ execParser (info cmdOpts idm)

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     ["cspoppy-load"]        -> runCsPoppyBuild
--     ["cspoppy-select-all"]  -> runCsPoppySelectAll
--     ["poppy512-load"]       -> runPoppy512Build
--     ["poppy512-select-all"] -> runPoppy512SelectAll
--     _                       -> putStrLn "Invalid arguments"
