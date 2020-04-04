{-|
Module: Fibonacci Helper
Description: Fibonacci Helper Functions

Helper functions for the handling of Fibonacci series and Fibonacci
codes. See:

 * https://wiki.haskell.org/The_Fibonacci_sequence
 * https://wiki.haskell.org/The_Fibonacci_sequence
-}

module Fibonacci ( fibonacciSeries
                 ) where

-- | Fibonacci Series [https://oeis.org/A000045].
fibonacciSeries :: [Integer]
fibonacciSeries = 1 : 1 : zipWith (+) fibonacciSeries (tail fibonacciSeries)
