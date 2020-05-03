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

import Data.Bits (testBit)
-- This file contains the building blocks for Elias Gamma
-- codes. Remember that ones are followed by a zero for length
-- determination.
import Data.Bits.Coded
import Data.Bits.Coding (Coding, putBit, putBits)
import Data.Word
import Data.Bytes.Put (runPutL, flush, putWord8)
import Data.Binary.Put (PutM)
import qualified Data.ByteString.Lazy as BL

-- | Encode Words into Elias Gamma Codes and write them into a ByteString. Remember that putting a zero will cause undesired results.
w16ToStream :: [Word16] -- ^ List of words
            -> BL.ByteString -- ^ Elias Gamma encodes stream.
w16ToStream xs = pf $ do encodeMany encs
                         flush
  where encs = map Elias xs :: [Gamma Word16 Word16]
        pf = runPutL . runEncode


-- | Encode reccuring characters and prefix them with a count
-- information encoded in Elias Gamma.
vlcRleComp :: BL.ByteString -> BL.ByteString
vlcRleComp b = run $ zip bgs ls'
  where bgs = BL.group b -- Group the ByteString to get recurring bytes.
        ls = map BL.length bgs -- Get length of elements
        ls' = map (Elias . fromIntegral) ls :: [Gamma Word16 Word16]
        run :: [(BL.ByteString, Gamma Word16 Word16)] -> BL.ByteString
        run xs = runPutL $ runEncode $ mapM_ (uncurry f) xs >> flush
        f :: BL.ByteString -> Gamma Word16 Word16 -> Coding PutM ()
        -- TODO: Currently does not work as the stream is flushed after each count information!
        f x l = do encode l
                   mapM_ (putBit . testBit (BL.head x)) [7,6..0]
                   -- Howto use? putBits 0 16 (0x5555::Word16)

