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
  codeword :: Word8
  } deriving (Show, Data, Typeable)

-- The generated command line is not compatible with getopt!!!
-- "cruncher -c=9 ..." has to be used! Otherwise the default value is
-- used and 9 is an additional argument.
--
-- OK, apparently I misread the documentation... It could be clearer
-- here... Have a look at
-- https://spin.atomicobject.com/2012/12/13/using-haskells-cmdargs-package/. So,
-- def is a "default" value for the specified type.
-- 
cliparam = Cruncher{
  files = def &= args &= typ "FILES/DIRS",
  codeword = 0xFA &= help "Codeword (BYTE) for the initiation of the rle sequence" &= opt (0xFA::Word8)
  }
  &= summary "Cruncher with (in future) several algorithms."


crunchFile :: (BL.ByteString -> BL.ByteString) -- ^ A function compressing a ByteString into another ByteString
           -> FilePath -- ^ The filename to compress
           -> IO ()
crunchFile cf fn = do
  fcontens <- BL.readFile fn
  printf "Crunching `%50s`...\n" fn
  BL.writeFile (fn ++ ".crunched") $ cf fcontens


run :: Cruncher -> IO ()
run cpar
  | length (files cpar) == 0 = putStrLn "Error! No files supplied!"
  | otherwise = let cf = rleCrunchCodeword (codeword cpar)
                in do mapM_ (crunchFile cf) $ files cpar

-- | Call the cruncher and compress files.
main :: IO ()
main = do cpar <- cmdArgs cliparam
          -- print cpar
          run cpar
