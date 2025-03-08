module App.Cli.Commands.BalancedParens
  ( cmdBalancedParens
  ) where

import App.Cli.Commands.BalancedParens.BitsToParens
import App.Cli.Commands.BalancedParens.ParensToBits
import App.Cli.Commands.BalancedParens.Positions
import Options.Applicative
import Options.Applicative qualified as Opt

{- HLINT ignore "Monoid law, left identity" -}

subParser :: String -> ParserInfo a -> Parser a
subParser availableCommand pInfo =
  Opt.hsubparser $ Opt.command availableCommand pInfo <> Opt.metavar availableCommand

cmdBalancedParens :: Parser (IO ())
cmdBalancedParens =
  subParser "balanced-parens" $ optInfo $ Opt.progDesc "Balanced Parens"
  where
    optInfo =
      Opt.info $ subparser $ mconcat
        [ cmdParensToBits
        , cmdBitsToParens
        , cmdPositions
        ]
