module Data.Succinct.Simd.ChunkString
  ( ChunkString
  , ToChunkString(..)
  , hGetContents
  ) where

import Data.Succinct.Simd.Internal.ChunkString

import qualified HaskellWorks.Data.ByteString as BS
import qualified System.IO                    as IO

hGetContents :: IO.Handle -> IO ChunkString
hGetContents = (ChunkString <$>) . BS.hGetContentsChunkedBy chunkSize
{-# INLINE hGetContents #-}
