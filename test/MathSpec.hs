{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module MathSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding (NonZero, NonNegative)
import Control.Exception (evaluate)
import qualified HaskPy.Math.Math as Math
import HaskPy.Math.Math (add, subtract', multiply, divide, fib)

-- Helper function for testing division
divide' :: Double -> Double -> Either String Double
divide' = divide

-- | Newtype wrapper for non-zero values for testing
newtype NonZero a = NonZero { unNonZero :: a }
  deriving (Eq, Show)

-- | Arbitrary instance for NonZero
instance (Num a, Eq a, Arbitrary a) => Arbitrary (NonZero a) where
  arbitrary = do
    x <- arbitrary `suchThat` (/= 0)
    return $ NonZero x

-- | Newtype wrapper for non-negative integers for testing
newtype NonNegativeInt = NonNegativeInt { unNonNegativeInt :: Int }
  deriving (Eq, Show)

-- | Arbitrary instance for NonNegativeInt
instance Arbitrary NonNegativeInt where
  arbitrary = NonNegativeInt . abs <$> arbitrary

spec :: Spec
spec = do
  describe "add" $ do
    it "adds two numbers" $ do
      add (1 :: Int) (2 :: Int) `shouldBe` (3 :: Int)
      add (-1 :: Int) (1 :: Int) `shouldBe` (0 :: Int)
    
    it "is commutative" $ property $ \(x :: Int) (y :: Int) -> 
      add x y === add y x
    
    it "has identity element 0" $ property $ \(x :: Int) ->
      add x 0 === x

  describe "subtract'" $ do
    it "subtracts the second number from the first" $ do
      subtract' (5 :: Int) (3 :: Int) `shouldBe` (2 :: Int)
      subtract' (3 :: Int) (5 :: Int) `shouldBe` (-2 :: Int)

  describe "subtract" $ do
    it "subtracts two numbers" $ do
      Math.subtract (5 :: Int) (3 :: Int) `shouldBe` (2 :: Int)
    it "is the inverse of addition" $ property $ \(x :: Int) (y :: Int) ->
      Math.subtract (add x y) y `shouldBe` (x :: Int)

  describe "multiply" $ do
    it "multiplies two numbers" $ do
      multiply (2 :: Int) (3 :: Int) `shouldBe` (6 :: Int)
      multiply (0 :: Int) (5 :: Int) `shouldBe` (0 :: Int)
    
    it "is commutative" $ property $ \(x :: Int) (y :: Int) -> 
      multiply x y === multiply y x
    
    it "has identity element 1" $ property $ \(x :: Int) ->
      multiply x 1 === x

    it "returns 0 when multiplying by 0" $ property $ \(x :: Int) ->
      multiply x 0 `shouldBe` 0

  describe "divide" $ do
    it "divides two numbers" $ do
      let result = divide' 10.0 2.0
      case result of
        Left _ -> expectationFailure "Expected Right but got Left"
        Right r -> r `shouldBe` 5.0
    
    it "returns Left for division by zero" $ do
      let result = divide' 10.0 0.0
      case result of
        Left err -> err `shouldBe` ("Division by zero" :: String)
        Right _ -> expectationFailure "Expected Left but got Right"
    
    it "is the inverse of multiplication" $ property $ \(y' :: Double) ->
      y' /= 0 ==> 
        let x = 10.0 :: Double
            result = divide (x * y') y'
            -- Use approximate equality for floating point values
            approxEqual a b = abs (a - b) < 1e-9
        in case result of
             Right val -> val `approxEqual` x `shouldBe` True
             Left err -> expectationFailure $ "Division failed: " ++ err

  describe "fib" $ do
    it "returns correct Fibonacci numbers" $ do
      map (fib :: Int -> Integer) [0 .. 10] `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
    it "is defined for large indices" $ do
      fib (50 :: Int) `shouldBe` (12586269025 :: Integer)
    it "throws error for negative indices" $ do
      evaluate (fib (-1 :: Int) :: Integer) `shouldThrow` anyErrorCall
    it "satisfies the Fibonacci recurrence relation" $ property $
      \(n :: Int) -> 
        let n' = abs n `mod` 20  -- Limit size to prevent stack overflow
            fib' :: Int -> Integer
            fib' = fib
        in n' > 1 ==> fib' n' === fib' (n' - 1) + fib' (n' - 2)
