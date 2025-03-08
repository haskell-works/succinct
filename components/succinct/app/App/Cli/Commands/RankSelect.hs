module App.Cli.Commands.RankSelect
  ( cmdRankSelect
  ) where

import App.Cli.Commands.RankSelect.Build
import App.Cli.Commands.RankSelect.SelectAll
import App.Cli.Commands.RankSelect.UnitTest
import App.Cli.Commands.RankSelect.Validate
import Options.Applicative
import Options.Applicative qualified as Opt

{- HLINT ignore "Monoid law, left identity" -}

subParser :: String -> ParserInfo a -> Parser a
subParser availableCommand pInfo =
  Opt.hsubparser $ Opt.command availableCommand pInfo <> Opt.metavar availableCommand

cmdRankSelect :: Parser (IO ())
cmdRankSelect =
  subParser "rank-select" $ optInfo $ Opt.progDesc "Rank Select"
  where
    optInfo =
      Opt.info $ subparser $ mconcat
        [ cmdBuild
        , cmdSelectAll
        , cmdValidate
        , cmdUnitTest
        ]
