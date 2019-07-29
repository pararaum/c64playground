module RLECruncher ( groupdat
                   , rleCrunchCodeword
                   ) where

import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Word (Word8)


data Drun = Drun { drunLength :: Word8
                 , value :: Word8
                 } deriving (Show)

rleEncode :: [(Int, Word8)] -> [Drun]
rleEncode ((x, y):zs) | x < 256 = Drun (fromIntegral x) y : rleEncode zs
                      | otherwise = Drun 255 y : rleEncode ((x - 255, y) : zs)
rleEncode [] = []

rleOut :: Word8 -> [Drun] -> [Word8]
rleOut cw (x:xs) | drunLength x == 1 && value x /= cw = value x : rleOut' xs
                 | drunLength x == 1 && value x == cw = cw : 1 : cw : rleOut' xs
                 | drunLength x == 2 && value x /= cw = value x : value x : rleOut' xs
                 | otherwise = cw : drunLength x : value x : rleOut' xs
  where rleOut' = rleOut cw
rleOut _ [] = []

groupdat :: BL.ByteString -> [(Int, Word8)]
groupdat = map f . BL.group
  where f x = (fromIntegral $ BL.length x, BL.head x)


-- |Compress a ByteString and get the rle compressed bytestring where the code word version is used.
rleCrunchCodeword :: Word8               -- ^ The code word to use
                 -> BL.ByteString       -- ^ The ByteString to compress
                 -> BL.ByteString       -- ^ Compressed ByteString
rleCrunchCodeword x = BL.pack . rleOut x . rleEncode . groupdat
