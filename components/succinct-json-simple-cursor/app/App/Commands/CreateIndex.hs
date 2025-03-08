{-# LANGUAGE BangPatterns #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Maybe
import Data.Word
import Foreign
import Options.Applicative       hiding (columns)

import qualified App.Commands.Types                         as Z
import qualified Data.ByteString.Internal                   as BSI
import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.Succinct.Json.Simple.Cursor.SemiIndex as SISI
import qualified HaskellWorks.Data.ByteString.Lazy          as LBS
import qualified System.IO.MMap                             as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}

runCreateIndex :: Z.CreateIndexOptions -> IO ()
runCreateIndex opts = do
  let filePath = opts ^. the @"filePath"
  let outputIbFile = opts ^. the @"outputIbFile" & fromMaybe (filePath <> ".ib.idx")
  let outputBpFile = opts ^. the @"outputBpFile" & fromMaybe (filePath <> ".bp.idx")
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filePath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  let SISI.SemiIndex _ ibs bps = SISI.buildSemiIndex bs
  LBS.writeFile outputIbFile (LBS.toLazyByteString ibs)
  LBS.writeFile outputBpFile (LBS.toLazyByteString bps)

optsCreateIndex :: Parser Z.CreateIndexOptions
optsCreateIndex = Z.CreateIndexOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input JSON file"
        <>  metavar "STRING"
        )
  <*> strOption
        (   long "backend"
        <>  short 'b'
        <>  value "standard"
        <>  help "Backend for creating index"
        <>  metavar "STRING"
        )
  <*> optional
        ( strOption
          (   long "output-ib-file"
          <>  help "Filename for output ib index"
          <>  metavar "STRING"
          )
        )
  <*> optional
        ( strOption
          (   long "output-bp-file"
          <>  help "Filename for output bp index"
          <>  metavar "STRING"
          )
        )

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
