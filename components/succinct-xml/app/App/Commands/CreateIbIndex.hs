{-# LANGUAGE OverloadedStrings #-}

module App.Commands.CreateIbIndex
  ( cmdCreateIbIndex
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Succinct.Xml.Internal.ToIbBp64
import Data.Succinct.Xml.Succinct.Cursor.BlankedXml
import Options.Applicative                              hiding (columns)

import qualified App.Commands.Types   as Z
import qualified Data.ByteString.Lazy as LBS

runCreateIbIndex :: Z.CreateIbIndexOptions -> IO ()
runCreateIbIndex opt = do
  let input   = opt ^. the @"input"
  let output  = opt ^. the @"output"

  lbs <- LBS.readFile input
  let blankedXml  = lbsToBlankedXml lbs
  let ib          = toInterestBits64 blankedXml
  LBS.writeFile output (LBS.fromChunks ib)

optsCreateIbIndex :: Parser Z.CreateIbIndexOptions
optsCreateIbIndex = Z.CreateIbIndexOptions
  <$> strOption
      (   long "input"
      <>  help "Input file"
      <>  metavar "FILE"
      )
  <*> strOption
      (   long "output"
      <>  help "Interest Bits output"
      <>  metavar "FILE"
      )

cmdCreateIbIndex :: Mod CommandFields (IO ())
cmdCreateIbIndex = command "create-ib-index"  $ flip info idm $ runCreateIbIndex <$> optsCreateIbIndex
