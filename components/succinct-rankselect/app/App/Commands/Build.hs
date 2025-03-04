{-# LANGUAGE BangPatterns     #-}


module App.Commands.Build
  ( cmdBuild
  ) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import HaskellWorks.Data.FromForeignRegion
import Options.Applicative
import System.Directory

import qualified App.Commands.Options.Type                        as Z
import qualified Data.List                                        as L
import qualified Data.Succinct.RankSelect.CsPoppy                 as CS
import qualified Data.Succinct.RankSelect.CsPoppy.Internal.Alpha1 as A1
import qualified Data.Succinct.RankSelect.Poppy512                as P512

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

runBuild :: Z.BuildOptions -> IO ()
runBuild opts = case opts ^. the @"indexType" of
  Z.CsPoppy  -> do
    entries <- getDirectoryContents "components/succinct-rankselect/data"
    let files = ("components/succinct-rankselect/data/" ++) <$> (".ib" `L.isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      putStrLn $ "Loading cspoppy for " <> file
      CS.CsPoppy !_ !_ (A1.CsPoppyIndex !_ !_) <- mmapFromForeignRegion file
      return ()
  Z.Poppy512 -> do
    entries <- getDirectoryContents "components/succinct-rankselect/data"
    let files = ("components/succinct-rankselect/data/" ++) <$> (".ib" `L.isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      putStrLn $ "Loading cspoppy for " <> file
      P512.Poppy512 !_ !_ <- mmapFromForeignRegion file
      return ()

optsBuild :: Parser Z.BuildOptions
optsBuild = Z.BuildOptions
  <$> option auto
      (   long "index-type"
      <>  help "Index type"
      <>  metavar "INDEX_TYPE"
      )

cmdBuild :: Mod CommandFields (IO ())
cmdBuild = command "build"  $ flip info idm $ runBuild <$> optsBuild
