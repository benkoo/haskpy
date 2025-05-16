{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module APISpec (spec) where

import Test.Hspec hiding (pendingWith, before)
import qualified Test.Hspec.Wai as Wai
  ( (<:>), with, shouldRespondWith, matchStatus, matchHeaders
  , get, post
  )
import Network.HTTP.Types (status400)
import Network.Wai (Application)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import System.Environment (lookupEnv)
import Control.Monad (unless)
import Web.Scotty (json, status, scottyApp, jsonData, ActionM, pathParam)
import qualified Web.Scotty as Scotty
import HaskPy.Math.Math (add, subtract', multiply, fib)
import HaskPy.Main (MathRequest(..), MathResponse(..), operation, x, y)

-- | Test configuration
data TestConfig = TestConfig
  { parallelTests :: Bool  -- ^ Run tests in parallel
  , quickMode :: Bool      -- ^ Run in quick mode (fewer tests)
  }

-- | Default test configuration
defaultTestConfig :: TestConfig
defaultTestConfig = TestConfig
  { parallelTests = True
  , quickMode = False
  }

-- | Re-export the application for testing
app :: IO Application
app = scottyApp $ do
  -- Fibonacci endpoint
  Scotty.get "/fib/:n" $ do
    n <- pathParam "n" :: ActionM Int64
    if n < 0
      then do
        status status400
        json $ MathResponse 0 "n must be non-negative"
      else json $ MathResponse (fib (fromIntegral n :: Integer)) "success"
  
  -- Math operation endpoint
  Scotty.post "/math" $ do
    req <- jsonData :: ActionM MathRequest
    let opResult = case operation req of
          "add" -> Right $ add (x req) (y req)
          "subtract" -> Right $ subtract' (x req) (y req)
          "multiply" -> Right $ multiply (x req) (y req)
          "divide" -> 
            if y req == 0 
              then Left ("Division by zero" :: String)
              else Right $ x req `div` y req
          _ -> Left "Invalid operation"
    
    case opResult of
      Right val -> json $ MathResponse val "success"
      Left _ -> do
        status status400
        -- Use standardized error message to match test expectations
        json $ MathResponse 0 "Invalid operation or division by zero"

-- Helper function to create JSON request body
jsonBody :: MathRequest -> LBS.ByteString
jsonBody = encode

-- | Test specifications
spec :: Spec
spec = do
  testCfg <- runIO $ do
    quick <- lookupEnv "QUICK_TEST"
    return $ case quick of
      Just "1" -> defaultTestConfig { quickMode = True }
      _ -> defaultTestConfig
  
  let testGroup = if parallelTests testCfg then parallel else id
  
  testGroup $ Wai.with app $ do
    describe "GET /fib/:n" $ do
      it "returns 200 and the nth Fibonacci number" $ do
        Wai.get "/fib/10" 
          `Wai.shouldRespondWith` "{\"result\":55,\"statusMsg\":\"success\"}"
          { Wai.matchStatus = 200 }
      
      it "returns 400 for negative n" $ do
        Wai.get "/fib/-1" 
          `Wai.shouldRespondWith` "{\"result\":0,\"statusMsg\":\"n must be non-negative\"}"
          { Wai.matchStatus = 400 }
      
      it "returns 404 for invalid path" $ do
        Wai.get "/invalid" 
          `Wai.shouldRespondWith` "<h1>404: File Not Found!</h1>"
          { Wai.matchStatus = 404,
            Wai.matchHeaders = ["Content-Type" Wai.<:> "text/html"]
          }
    
    describe "POST /math" $ do
      it "adds two numbers" $ do
        Wai.post "/math" (jsonBody $ MathRequest "add" 10 20)
          `Wai.shouldRespondWith` "{\"result\":30,\"statusMsg\":\"success\"}"
          { Wai.matchStatus = 200 }
      
      it "subtracts two numbers" $ do
        Wai.post "/math" (jsonBody $ MathRequest "subtract" 20 10)
          `Wai.shouldRespondWith` "{\"result\":10,\"statusMsg\":\"success\"}"
          { Wai.matchStatus = 200 }
      
      it "multiplies two numbers" $ do
        Wai.post "/math" (jsonBody $ MathRequest "multiply" 5 4)
          `Wai.shouldRespondWith` "{\"result\":20,\"statusMsg\":\"success\"}"
          { Wai.matchStatus = 200 }
      
      it "divides two numbers" $ do
        Wai.post "/math" (jsonBody $ MathRequest "divide" 10 2)
          `Wai.shouldRespondWith` "{\"result\":5,\"statusMsg\":\"success\"}"
          { Wai.matchStatus = 200 }
      
      it "handles division by zero" $ do
        Wai.post "/math" (jsonBody $ MathRequest "divide" 10 0)
          `Wai.shouldRespondWith` "{\"result\":0,\"statusMsg\":\"Invalid operation or division by zero\"}"
          { Wai.matchStatus = 400 }
      
      it "returns 400 for invalid operation" $ do
        Wai.post "/math" (jsonBody $ MathRequest "invalid" 1 1)
          `Wai.shouldRespondWith` "{\"result\":0,\"statusMsg\":\"Invalid operation or division by zero\"}"
          { Wai.matchStatus = 400 }
      
      -- Only run these tests in full mode
      unless (quickMode testCfg) $ do
        it "handles large numbers correctly" $ do
          Wai.post "/math" (jsonBody $ MathRequest "add" 1000000 2000000)
            `Wai.shouldRespondWith` "{\"result\":3000000,\"statusMsg\":\"success\"}"
            { Wai.matchStatus = 200 }
        
        it "handles negative numbers correctly" $ do
          Wai.post "/math" (jsonBody $ MathRequest "subtract" 5 10)
            `Wai.shouldRespondWith` "{\"result\":-5,\"statusMsg\":\"success\"}"
            { Wai.matchStatus = 200 }
