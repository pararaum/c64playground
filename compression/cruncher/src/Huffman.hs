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

import Data.Int
import Data.Word
import Data.Maybe
import Data.List
import Control.Arrow (second, (&&&))
import Data.Array.Unboxed
import qualified Data.PQueue.Min as PM

-- | A Huffman Tree is a special type of tree. Here the weight is
-- added so that it can be accessed easily. This is not the canonical
-- form.
data HuffmanTree
  = Leaf { leafWeight :: Int
         , leafWord :: Word8
         }
  | Branch { branchWeight :: Int
           , branchLeft :: HuffmanTree
           , branchRight :: HuffmanTree
           }
  deriving (Show, Eq)

-- | Add a Ord intance so the a priority queue can be used.
instance Ord HuffmanTree where
  compare (Leaf a _) (Leaf b _) = compare a b
  compare (Branch a _ _) (Leaf b _) = compare a b
  compare (Leaf a _) (Branch b _ _) = compare a b
  compare (Branch a _ _) (Branch b _ _) = compare a b

-- | Helper function to be called on a (subpart) of a Huffman
-- tree. This give the weight of this Branch or Leaf.
treeWeight :: HuffmanTree -> Int
treeWeight (Leaf a _) = a
treeWeight (Branch a _ _) = a

-- | Create a nice (LISP like) representation of the tree. IT is very
-- nice that there are that many unicode characters.
huffmanTreeToString :: HuffmanTree -> String
huffmanTreeToString (Leaf we wo) = "❲" ++ show we ++ "," ++ show wo ++ "❳"
huffmanTreeToString (Branch _ a b) = "❨" ++ huffmanTreeToString a ++ " " ++ huffmanTreeToString b ++ "❩"

-- | Calculate the depth of a Huffman Tree. This will calculate the
-- maximum depth of the current Huffman tree.
huffmanTreeDepth :: HuffmanTree -> Int
huffmanTreeDepth ht = f 0 ht
  where f n (Leaf _ _) = n
        f n (Branch _ x y) = let n' = n + 1
                             in max (f n' x) (f n' y)

-- | Convert a Huffman tree into an array. The tree starts at element
-- 1. If we are at position n in the tree the left child element can
-- be found at position 2n and the right child element at position
-- 2n+1.
huffmanTreeToArray :: HuffmanTree -> UArray Int Int
huffmanTreeToArray ht = f na 1 ht
  where na = listArray (0, 1+2^(huffmanTreeDepth ht + 1)) $ repeat (-32768) :: UArray Int Int
        f a i (Leaf _ x) = a // [(i, fromIntegral x)]
        f a i (Branch _ x y) = l // (filter ((/= -32768) . snd) $ assocs r)
          where l = f a (2 * i) x
                r = f a (2 * i + 1) y

-- | Function to calculate the frequency of symbols. 
freq :: Ord a => [a] -- ^ A list of symbols.
    -> [(Int, a)] -- ^ Returns a sorted list of count, symbol pairs.
freq = fmap (length &&& head) . group . sort
 
freqToPQeue = PM.fromList . map (uncurry Leaf)

-- | Build the tree from a priority consisting of Leaf elemens
-- only. To build a tree put all elements from the frequency
-- calculation into a priority queue. This function will build a tree
-- from this using a priority queue. The elements are combined until
-- there is only one element (the root) left.
buildTree :: PM.MinQueue HuffmanTree -> HuffmanTree
buildTree x | PM.size x == 1 = fromJust $ PM.getMin x
            | otherwise = let (min1, x') = PM.deleteFindMin x
                              (min2, x'') = PM.deleteFindMin x'
                              newbr = Branch (treeWeight min1 + treeWeight min2) min1 min2
                          in buildTree $ PM.insert newbr x''

-- | Serialise the tree into a list of associations. This gives the
-- corresponding strings to each value in the Huffman tree.
serialize :: HuffmanTree -> [(Word8, String)]
serialize (Branch _ l r) =
  (second ('0' :) <$> serialize l) ++ (second ('1' :) <$> serialize r)
serialize (Leaf _ x) = [(x, "")]
