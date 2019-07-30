{-|
Module: Huffman
Description: Huffman-style compression and helper functions.

A Huffman compression (and variants are implemented). There are, of
course, the default library functions as in
https://hackage.haskell.org\/package\/huffman but here we would like
to do out own implementation for learning purposes.

Links:

 * https://rosettacode.org\/wiki\/Huffman_coding#Haskell
 * https://hackage.haskell.org\/package\/huffman
-}
module Huffman ( freq
               ) where

import Data.List
import Control.Arrow ((&&&))


-- | Function to calculate the frequency of symbols. 
freq :: Ord a => [a] -- ^ A list of symbols.
    -> [(Int, a)] -- ^ Returns a sorted list of count, symbol pairs.
freq = fmap (length &&& head) . group . sort
 
