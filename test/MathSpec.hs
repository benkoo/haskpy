module MathSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import HaskPy.Math

spec :: Spec
spec = do
  describe "add" $ do
    it "adds two numbers" $ do
      add 1 2 `shouldBe` (3 :: Int)
      add (-1) 1 `shouldBe` (0 :: Int)
    it "is commutative" $ property $ \x y -> 
      add x y === add (y :: Int) (x :: Int)
    it "has identity element 0" $ property $ \x ->
      add x 0 === (x :: Int)

  describe "subtract'" $ do
    it "subtracts the second number from the first" $ do
      subtract' 5 3 `shouldBe` (2 :: Int)
      subtract' 3 5 `shouldBe` (-2 :: Int)

  describe "multiply" $ do
    it "multiplies two numbers" $ do
      multiply 2 3 `shouldBe` (6 :: Int)
      multiply 0 5 `shouldBe` (0 :: Int)
    it "is commutative" $ property $ \x y -> 
      multiply x y === multiply (y :: Int) (x :: Int)
    it "has identity element 1" $ property $ \x ->
      multiply x 1 === (x :: Int)

  describe "divide" $ do
    it "divides two numbers" $ do
      divide 6 2 `shouldBe` Just (3 :: Double)
      divide 1 0 `shouldBe` (Nothing :: Maybe Double)
    it "is not defined for division by zero" $ property $ \x ->
      divide x (0 :: Double) === (Nothing :: Maybe Double)

  describe "fib" $ do
    it "returns correct Fibonacci numbers" $ do
      map fib [0..10] `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
    it "is defined for large indices" $ do
      fib 50 `shouldBe` 12586269025
    it "raises an error for negative indices" $ do
      evaluate (fib (-1)) `shouldThrow` anyErrorCall
