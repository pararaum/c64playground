{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-|
Module: Main (cruncher)
Description: Diverse cruncher for data
License: GPL-2

Cruncher program to crunch binary file. All files given on the command
line are crunched and written to the file with an added extension of
"crunched".
-}
module Main where
import qualified Data.ByteString.Lazy as BL
import System.Console.CmdArgs
import Control.Monad
import Text.Printf
import Data.Word
import RLECruncher

-- This cruncher program will compress data (aka "crunch") data and
-- write it into stdout. This can then be used in the C64 program
-- accordingly.

data Cruncher = Cruncher {
  files :: [FilePath],
  codeword :: Word8,
  crap :: String
  } deriving (Show, Data, Typeable)

-- The generated command line is not compatible with getopt!!!
-- "cruncher -c=9 ..." has to be used! Otherwise the default value is
-- used and 9 is an additional argument.
--
-- It is even worse...
-- 
-- No, this is complete crap. Who came up with the idea of having a
-- standard default value and being able to give an optional
-- argument. How is it possible to have a non-zero default value for
-- an argument and being able to provide a different value using the
-- command line? This use case does make much more sense the the
-- currently implemented one.
cliparam = Cruncher{
  files = def &= args &= typ "FILES/DIRS",
  codeword = def &= help "Codeword (BYTE) for the initiation of the rle sequence" &= opt (0xFA::Word8),
  crap = auto &= opt "crap"
  }
  &= summary "cruncher with several algorithms"


crunchFile :: FilePath -> IO ()
crunchFile fn = do
  fcontens <- BL.readFile fn
  printf "Crunching `%50s`...\n" fn
  BL.writeFile (fn ++ ".crunched") $ rleCrunchCodeword 0xfa fcontens

-- | Call the cruncher and compress files.
main :: IO ()
main = do cpar <- cmdArgs cliparam
          print $ codeword cpar
          print $ crap cpar
          mapM_ crunchFile $ files cpar
