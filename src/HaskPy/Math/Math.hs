module HaskPy.Math.Math
  ( add
  , subtract'
  , subtract
  , multiply
  , divide
  , fib
  ) where

import Prelude (Num, Integral, Eq, RealFloat, (+), (-), (*), (/), abs, (<), otherwise, error)
import Data.Either (Either(..))
import Data.String (String)
import HaskPy.Math.Internal (fibMemo)

-- | Add two numbers
add :: Num a => a -> a -> a
add = (+)

-- | Subtract the second number from the first
subtract' :: Num a => a -> a -> a
subtract' = (-)

-- | Subtract the second number from the first (same as subtract')
subtract :: Num a => a -> a -> a
subtract x y = x - y

-- | Multiply two numbers
multiply :: Num a => a -> a -> a
multiply = (*)

-- | Divide two numbers, returns Left with error message for division by zero
-- Uses a small epsilon for floating point comparisons to avoid precision issues
divide :: (Eq a, RealFloat a) => a -> a -> Either String a
divide _ 0 = Left "Division by zero"
divide a b
  | abs b < 1e-10 = Left "Division by zero" -- Handle values very close to zero
  | otherwise = Right (a / b)

-- | Calculate the nth Fibonacci number using efficient memoization
-- Uses a fast matrix exponentiation algorithm for O(log n) performance
fib :: (Integral p, Num a) => p -> a
fib n
  | n < 0 = error "Fibonacci is not defined for negative indices"
  | otherwise = fibMemo n
