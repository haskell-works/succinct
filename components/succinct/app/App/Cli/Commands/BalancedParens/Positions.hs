module App.Cli.Commands.BalancedParens.Positions (
    cmdPositions,
) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Succinct.BalancedParens.FindClose
import Data.Succinct.BalancedParens.OpenAt
import Data.Word
import HaskellWorks.Data.Positioning
import Options.Applicative                    hiding (columns)

import App.Cli.Options.Type                qualified as Z
import Data.Vector.Storable                qualified as DVS
import HaskellWorks.Data.FromForeignRegion qualified as IO
import System.IO                           qualified as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant return"    -}

openCloses :: (FindClose v, OpenAt v) => v -> [(Count, Count)]
openCloses v = go 1 v []
  where
    go :: (FindClose v, OpenAt v) => Count -> v -> [(Count, Count)] -> [(Count, Count)]
    go p w =
        if openAt w p
            then case findClose w p of
                Just q  -> ((p, q) :) . go (p + 1) w . go (q + 1) w
                Nothing -> id
            else id

runPositions :: Z.PositionsOptions -> IO ()
runPositions opts = do
    let inputFile = opts ^. the @"inputFile"

    v :: DVS.Vector Word64 <- IO.mmapFromForeignRegion inputFile

    forM_ (openCloses v) $ \(o, c) -> do
        IO.putStrLn $ show o <> "," <> show c

    return ()

optsPositions :: Parser Z.PositionsOptions
optsPositions =
    Z.PositionsOptions
        <$> strOption
            ( long "input"
                <> help "Input file"
                <> metavar "FILE"
            )

cmdPositions :: Mod CommandFields (IO ())
cmdPositions = command "positions" $ flip info idm $ runPositions <$> optsPositions
