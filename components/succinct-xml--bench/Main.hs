{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- HLINT ignore "Use ++" -}

module Main where

import Criterion.Main
import Data.ByteString                         (ByteString)
import Data.Word
import Foreign
import HaskellWorks.Data.BalancedParens.Simple
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Xml.Internal.Blank
import HaskellWorks.Data.Xml.Internal.List
import HaskellWorks.Data.Xml.Internal.Tables
import HaskellWorks.Data.Xml.Succinct.Cursor
import System.IO.MMap

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS

setupEnvXml :: FilePath -> IO ByteString
setupEnvXml filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

loadXml :: ByteString -> XmlCursor ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
loadXml bs = fromByteString bs :: XmlCursor ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))

xmlToInterestBits3 :: [ByteString] -> [ByteString]
xmlToInterestBits3 = blankedXmlToInterestBits . blankXml

runCon :: ([i] -> [ByteString]) -> i -> ByteString
runCon con bs = BS.concat $ con [bs]

benchRankXmlCatalogLists :: [Benchmark]
benchRankXmlCatalogLists =
  [ env (setupEnvXml "components/succinct-xml--data/catalog.xml") $ \bs -> bgroup "catalog.xml"
    [ bench "Run blankXml"            (whnf (runCon blankXml          ) bs)
    , bench "Run xmlToInterestBits3"  (whnf (runCon xmlToInterestBits3) bs)
    , bench "loadXml"                 (whnf loadXml                     bs)
    ]
  ]

setupInterestingWord8s :: IO ()
setupInterestingWord8s = do
  let !_ = interestingWord8s
  return ()

benchIsInterestingWord8 :: [Benchmark]
benchIsInterestingWord8 =
  [ env setupInterestingWord8s $ \_ -> bgroup "Interesting Word8 lookup"
    [ bench "isInterestingWord8"  (whnf isInterestingWord8 0)
    ]
  ]

main :: IO ()
main = defaultMain $ concat
  [ benchIsInterestingWord8
  , benchRankXmlCatalogLists
  ]
