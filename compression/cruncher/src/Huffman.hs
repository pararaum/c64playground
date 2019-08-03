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

import Data.Word
import Data.Maybe
import Data.List
import Control.Arrow ((&&&))
import qualified Data.PQueue.Min as PM

data HuffmanTree
  = Leaf { leafWeight :: Int
         , leafWord :: Word8
         }
  | Branch { branchWeight :: Int
           , branchLeft :: HuffmanTree
           , branchRight :: HuffmanTree
           }
  deriving (Show, Eq)

instance Ord HuffmanTree where
  compare (Leaf a _) (Leaf b _) = compare a b
  compare (Branch a _ _) (Leaf b _) = compare a b
  compare (Leaf a _) (Branch b _ _) = compare a b
  compare (Branch a _ _) (Branch b _ _) = compare a b

treeWeight :: HuffmanTree -> Int
treeWeight (Leaf a _) = a
treeWeight (Branch a _ _) = a

huffmanTreeToString :: HuffmanTree -> String
huffmanTreeToString (Leaf we wo) = "❲" ++ show we ++ "," ++ show wo ++ "❳"
huffmanTreeToString (Branch _ a b) = "❨" ++ huffmanTreeToString a ++ " " ++ huffmanTreeToString b ++ "❩"

huffmanTreeDepth :: HuffmanTree -> Int
huffmanTreeDepth ht = f 0 ht
  where f n (Leaf _ _) = n
        f n (Branch _ x y) = let n' = n + 1
                             in max (f n' x) (f n' y)

-- | Function to calculate the frequency of symbols. 
freq :: Ord a => [a] -- ^ A list of symbols.
    -> [(Int, a)] -- ^ Returns a sorted list of count, symbol pairs.
freq = fmap (length &&& head) . group . sort
 
freqToPQeue = PM.fromList . map (uncurry Leaf)


buildTree :: PM.MinQueue HuffmanTree -> HuffmanTree
buildTree x | PM.size x == 1 = fromJust $ PM.getMin x
            | otherwise = let (min1, x') = PM.deleteFindMin x
                              (min2, x'') = PM.deleteFindMin x'
                              newbr = Branch (treeWeight min1 + treeWeight min2) min1 min2
                          in buildTree $ PM.insert newbr x''
  
