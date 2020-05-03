{-|
Module: UniversalCode
Description: Universal Code Encoding

Universal codes can be used to encode values into a variable
length. This can be used for data compression and also if the highest
number is not known in advance.

The Elias-Gamma-Codes used here use a stream of ones and the a zero
instead of a stream of zeros and a one as described in
https://en.wikipedia.org/wiki/Elias_gamma_coding.
-}

module UniversalCode ( w16ToStream
                     ) where

-- This file contains the building blocks for Elias Gamma
-- codes. Remember that ones are followed by a zero for length
-- determination.
import Data.Bits.Coded
import Data.Word
import Data.Bytes.Put (runPutL, flush)
import qualified Data.ByteString.Lazy as BL

-- | Encode Words into Elias Gamma Codes and write them into a ByteString. Remember that putting a zero will cause undesired results.
w16ToStream :: [Word16] -- ^ List of words
            -> BL.ByteString -- ^ Elias Gamma encodes stream.
w16ToStream xs = pf $ do encodeMany encs
                         flush
  where encs = map Elias xs :: [Gamma Word16 Word16]
        pf = runPutL . runEncode
