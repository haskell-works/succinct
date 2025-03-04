module Data.Succinct.Json.DecodeError where

newtype DecodeError = DecodeError String deriving (Eq, Show)
