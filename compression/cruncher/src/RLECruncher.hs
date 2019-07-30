{-|
Module: RLECruncher
Description: Run-Length Encoding

This module implements several RLE compression techniques. Currently
only a compression is done using run-length encoding, see
e.g. https://en.wikipedia.org/wiki/Run-length_encoding.

Currently only the variant is implement which will write a codeword to
mark the beginning of a compressed run, e.g. codeword, length, byte
value. Thus the maximum run length is 255 bytes. If the codeword
itself is in the original data then a run of one byte is encoded.
-}
module RLECruncher ( groupdat
                   , rleCrunchCodeword
                   ) where

import qualified Data.ByteString.Lazy as BL
import Data.List
import Data.Word (Word8)
import Control.Arrow ((&&&))


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


-- | From a ByteString get pair of (count, BYTE) is extracted. This
-- can be used in later stages to do the actual compression.
groupdat :: BL.ByteString -- ^ ByteString to count the occurences of the bytes.
         -> [(Int, Word8)] -- ^ Return count and BYTE value pairs.
groupdat = fmap (fromIntegral . BL.length &&& BL.head) . BL.group


-- |Compress a ByteString and get the rle compressed bytestring where the code word version is used.
rleCrunchCodeword :: Word8               -- ^ The code word to use
                 -> BL.ByteString       -- ^ The ByteString to compress
                 -> BL.ByteString       -- ^ Compressed ByteString
rleCrunchCodeword x = BL.pack . rleOut x . rleEncode . groupdat
