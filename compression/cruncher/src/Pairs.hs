{-|
Module: Pairs
Description: Pair compression.

Using the pair compression algorithm. This version and the tools
herein are optimised for later usage in the C64.

Links:
https://en.wikipedia.org/wiki/Byte_pair_encoding
-}
module Pairs ( getPairs,
               RePair (..)
             ) where


import Data.List
import Huffman (freq)

data RePair = RPLiteral Char
            | RPPair Int RePair RePair deriving (Show)

type RePairList = [RePair]

data RPState = RPState { dataStream :: [RePair]
                       , pairList :: [RePair]
                       } deriving (Show)

instance Eq RePair where
  (RPLiteral a) == (RPLiteral b) = a == b
  (RPLiteral _) == RPPair{} = False
  RPPair{} == (RPLiteral _) = False
  (RPPair a _ _) == (RPPair b _ _) = a == b
  
instance Ord RePair where
  (RPLiteral a) `compare` (RPLiteral b) = a `compare` b
  (RPLiteral a) `compare` (RPPair b _ _) = fromEnum a `compare` b
  (RPPair a _ _) `compare` (RPLiteral b) = a `compare` fromEnum b
  (RPPair a _ _) `compare` (RPPair b _ _) = a `compare` b


-- | Compress the stream using the pair-compression algorithm
compressPairs :: RPState -- ^ Original state with an empty pairList and the original dataStream to be compressed.
              -> RPState -- ^ Output of compressed stream and list of generated pairs.
compressPairs o@(RPState s l) = let freqs = getPairFreq s
                                    -- ^ Get the frequencies of the pairs
                                    (tc, tf) = head freqs
                                    -- ^ Get the highest frequency.
                                    np = insertPair tf l
                                    -- ^ Create a new pair using the old list of pairs.
                                    s' = replacePair np s
                                    -- ^ The stream with the pair of tokens with the top-most frequency replaced by the newly generated pair.
                                    l' = l ++ [np]
                                    -- ^ New list of tokens with the new token added.
                                in if not (null freqs) && (tc > 1) then compressPairs (RPState s' l') else o

getPairs :: Ord a => [a] -> [(a,a)]
getPairs [] = []
getPairs [x] = []
getPairs [x,y] = [(x,y)]
getPairs zzs@(x:y:zs) = (x,y) : getPairs (tail zzs)

getPairFreq :: Ord a => [a] -> [(Int, (a, a))]
getPairFreq = sortBy (flip compare) . freq . getPairs

insertPair :: (RePair, RePair) -> RePairList -> RePair
insertPair (x, y) ls = RPPair (256 + length ls) x y

-- replacePair__ :: RePair -> RePair -> [RePair] -> [RePair]
-- replacePair__ a b (x:y:zs) | a == x && b == y = RPPair rs a b : replacePair a b zs
--                            | otherwise = x : replacePair a b (y:zs)
--   where rs = -1
-- replacePair__ _ _ (x:[]) = [x]
-- replacePair :: RePair -> RePair -> RPState -> RPState
-- replacePair a b (RPState s l) = let np = insertPair (a, b) l
--                                     f (x:y:zs) | a == x && b == y = np : zs
--                                                | otherwise = x:y:zs
--                                     s' = s ++ [np]
--                                 in replacePair a b (RPState s' (tail $ f l))

replacePair :: RePair -> [RePair] -> [RePair]
replacePair n@(RPPair _ a b) (x:y:zs) | a == x && b == y = n : replacePair n zs
                                      | otherwise = x : replacePair n (y:zs)
replacePair _ [x] = [x]
replacePair _ [] = []

