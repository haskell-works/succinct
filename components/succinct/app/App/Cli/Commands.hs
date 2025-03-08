module App.Cli.Commands where

import App.Cli.Commands.BalancedParens
import App.Cli.Commands.RankSelect
import Options.Applicative

{- HLINT ignore "Monoid law, left identity" -}

cmds :: Parser (IO ())
cmds =
  asum 
    [ cmdBalancedParens
    , cmdRankSelect
    ]
