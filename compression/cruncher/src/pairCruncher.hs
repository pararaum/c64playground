{-|
Module: Pair Cruncher
Description: Using the pair algorithm to compress a file.
License: GPL-2

Using the pair-compressor algorithm (or something like it)
[https://en.wikipedia.org/wiki/Byte_pair_encoding]. A single C64 file
is loaded and crunched.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Data.Binary.Get
import Data.Word
import qualified Data.ByteString.Lazy as BL
import System.Console.CmdArgs


data Cruncher = Cruncher {
  files :: FilePath,
  executionAddress :: Word16
  } deriving (Show, Data, Typeable)

cliparam = Cruncher {
  files = def &= args &= typ "FILE",
  executionAddress = def &= help "Where to jump after decrunching"
  }
  &= summary "Cruncher with byte-pair encoding algorithm."

-- | Read the load address as from a C64 standard file.
readLoadAddress :: BL.ByteString -- ^ file contents
                -> Word16 -- ^ load address
readLoadAddress = runGet getWord16le

-- | Call the cruncher and compress files.
main :: IO ()
main = do cpar <- cmdArgs cliparam
          print cpar
          contents <- BL.readFile $ files cpar
          print $ readLoadAddress contents
          putStrLn "Cruncher 7777" 

