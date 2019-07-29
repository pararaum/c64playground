{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main where
import qualified Data.ByteString.Lazy as BL
import System.Console.CmdArgs
import Control.Monad
import RLECruncher

-- This cruncher program will compress data (aka "crunch") data and
-- write it into stdout. This can then be used in the C64 program
-- accordingly.

data Cruncher = Cruncher {
  files :: [FilePath]
  } deriving (Show, Data, Typeable)

cliparam = Cruncher{
  files = def &= args &= typ "FILES/DIRS"
  }
  &= summary "cruncher with several algorithms"


main :: IO ()
main = do cpar <- cmdArgs cliparam
          print cpar
          fcontents <- mapM BL.readFile $ files cpar
          mapM_ (BL.putStr . rleCrunchCodeword 0xfa) fcontents
