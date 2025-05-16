-- | Internal math utilities that are not exposed in the public API.
module HaskPy.Math.Internal
  ( fibs
  , fibMemo
  ) where

import Prelude (Num, Integral, Integer, (+), (-), (*), ($), zipWith, tail, fromIntegral, div, mod, (==), (<), error, otherwise, snd)

-- | Internal function to generate an infinite list of Fibonacci numbers
-- Note: This approach is inefficient for large indices because it recomputes values
fibs :: Num a => [a]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Efficient memoized Fibonacci implementation using dynamic programming
-- This implementation uses a fast matrix exponentiation approach which is O(log n)
fibMemo :: (Integral i, Num a) => i -> a
fibMemo n 
  | n == 0 = 0
  | n < 0 = error "Fibonacci is not defined for negative indices"
  | otherwise = snd $ fib' (fromIntegral n)
  where
    fib' :: Num a => Integer -> (a, a)
    fib' 0 = (1, 0)
    fib' 1 = (1, 1)
    fib' n' 
      | n' `mod` 2 == 0 = doubleStep (fib' (n' `div` 2))
      | otherwise = addStep (fib' (n' - 1))
    
    doubleStep :: Num a => (a, a) -> (a, a)
    doubleStep (f1, f0) = (f1*f1 + f0*f0, f0*(2*f1 - f0))
    
    addStep :: Num a => (a, a) -> (a, a)
    addStep (f1, f0) = (f1 + f0, f1)
