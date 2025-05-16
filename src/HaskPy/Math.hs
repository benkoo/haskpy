module HaskPy.Math
  ( add
  , subtract'
  , multiply
  , divide
  , fib
  ) where

-- | Add two numbers
add :: Num a => a -> a -> a
add a b = a + b

-- | Subtract the second number from the first
subtract' :: Num a => a -> a -> a
subtract' a b = a - b

-- | Multiply two numbers
multiply :: Num a => a -> a -> a
multiply a b = a * b

-- | Divide two numbers, returns Nothing for division by zero
divide :: (Fractional a, Eq a) => a -> a -> Maybe a
divide _ 0 = Nothing
divide a b = Just (a / b)

-- | Calculate the nth Fibonacci number
fib :: (Num a, Num b, Enum b) => b -> a
fib n = fibs !! fromIntegral n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
