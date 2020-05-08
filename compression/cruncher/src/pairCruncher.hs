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

import Data.Binary.Put (PutM)
import Data.Bits.Coded (runEncode)
import Data.Bits.Coding (Coding, putBit, putBits)
import Data.Bytes.Put (runPutL, flush)
import Data.Binary.Get
import Data.Word
import Math.NumberTheory.Logarithms (intLog2)
import qualified Data.ByteString.Lazy as BL
import System.Console.CmdArgs
import Text.Printf
import Pairs
import UniversalCode

data Cruncher = Cruncher {
  files :: FilePath,
  executionAddress :: Word16,
  maxLenPairList :: Int
  } deriving (Show, Data, Typeable)

cliparam = Cruncher {
  files = def &= args &= typ "FILE",
  executionAddress = def &= help "Where to jump after decrunching",
  maxLenPairList = 255 &= help "Maximum length of list of pairs"
  }
  &= summary "Cruncher with byte-pair encoding algorithm."

-- | Read the load address as from a C64 standard file.
readLoadAddress :: BL.ByteString -- ^ file contents
                -> Word16 -- ^ load address
readLoadAddress = runGet getWord16le

compPair :: Int -- ^ Maximum length of Pair List
         -> BL.ByteString -- ^ ByteString to compress
         -> RPState -- ^ Compressed result
compPair m c = compressPairsWMaxDepth m is
  where c' = BL.drop 2 c
        is = RPState (map RPLiteral $ BL.unpack c') [] -- initial state

testBits :: [Int] -> BL.ByteString
testBits xs = runPutL $ runEncode $ do mapM_ (putBits 11 0) xs
                                       flush

encodePairs :: Int -> RPState -> BL.ByteString
encodePairs mlpl s = runPutL $ runEncode $ do mapM_ pB pl' >> flush
  where pl = pairList s
        pl' = map rePairPair pl
        pB (x, y) = putBits b 0 x >> putBits b 0 y -- put the bits
        b = intLog2 $ mlpl + 256


encodePairsM :: Int -> RPState -> Coding PutM ()
encodePairsM mlpl s = mapM_ pB pl'
  where pl = pairList s
        pl' = map rePairPair pl
        pB (x, y) = putBits b 0 x >> putBits b 0 y -- put the bits
        b = intLog2 $ mlpl + 256


writeCrunch :: RPState -> String -> IO ()
writeCrunch s fn = do BL.writeFile fn d
  where d = runPutL $ runEncode $ encodePairsM 255 s

-- | Call the cruncher and compress files.
main :: IO ()
main = do cpar <- cmdArgs cliparam
          print cpar
          contents <- BL.readFile $ files cpar
          let compressed = compPair (maxLenPairList cpar) contents
          print $ readLoadAddress contents
          mapM_ print $ dataStream compressed
          print $ pairList compressed
          putStrLn $ printf "$%04X -> $%04X" (BL.length contents) (length $ dataStream compressed)
          putStrLn $ printf "%5d -> %5d" (BL.length contents) (length $ dataStream compressed)
          putStrLn $ printf "%s %d" "@PAIRS@" $ length $ pairList compressed
          BL.putStr $ encodePairs (maxLenPairList cpar) compressed
          putStrLn "Cruncher 7777" 
          BL.putStr $ testBits $ map rePairValue $ dataStream compressed
          writeCrunch compressed $ files cpar ++ ".crunched"
