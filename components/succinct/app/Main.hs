module Main where

import App.Cli.Commands
import Control.Monad
import Options.Applicative

main :: IO ()
main = join $ customExecParser
  (prefs $ showHelpOnEmpty <> showHelpOnError)
  (info (cmds <**> helper) idm)
