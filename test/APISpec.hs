module APISpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import Network.HTTP.Types (status400, status404, status200)
import Data.Aeson (encode)
import Data.ByteString.Lazy.Char8 (unpack)
import HaskPy.Math
import Main (MathRequest(..), MathResponse(..))
import Network.Wai (Application)
import Network.Wai.Test (SResponse(..))
import Web.Scotty (scottyApp, get, post, json, jsonData)
import qualified Web.Scotty as Scotty (status)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

-- Re-export the application for testing
app :: IO Application
app = scottyApp $ do
  -- Your application routes here
  get "/fib/:n" $ do
    n <- pathParam "n"
    if n < 0
      then do
        Scotty.status status400
        json $ MathResponse 0 "n must be non-negative"
      else json $ MathResponse (fromIntegral $ fib n) "success"
  
  post "/math" $ do
    req <- jsonData
    let mathResult = case operation req of
                      "add" -> Just $ add (x req) (y req)
                      "subtract" -> Just $ subtract' (x req) (y req)
                      "multiply" -> Just $ multiply (x req) (y req)
                      "divide" -> divide (x req) (y req)
                      _ -> Nothing
    
    case mathResult of
        Just val -> json $ MathResponse val "success"
        Nothing -> do
          Scotty.status status400
          json $ MathResponse 0 "Invalid operation or division by zero"

-- Helper function to create a JSON request body
jsonBody :: ToJSON a => a -> Request
jsonBody = flip setHeader jsonHeader . toStrict . encode
  where
    jsonHeader = ("Content-Type", "application/json")

toStrict = LBS.toStrict

spec :: Spec
spec = with app $ do
  describe "GET /fib/:n" $ do
    it "returns 200 and the nth Fibonacci number" $ do
      get "/fib/10" `shouldRespondWith` "{\"result\":55,\"statusMsg\":\"success\"}"
    
    it "returns 400 for negative numbers" $ do
      get "/fib/-1" `shouldRespondWith` 400
    
    it "returns 404 for invalid paths" $ do
      get "/invalid" `shouldRespondWith` 404
  
  describe "POST /math" $ do
    it "adds two numbers" $ do
      post "/math" (jsonBody $ MathRequest "add" 10 20) 
        `shouldRespondWith` "{\"result\":30.0,\"statusMsg\":\"success\"}"
    
    it "subtracts two numbers" $ do
      post "/math" (jsonBody $ MathRequest "subtract" 20 10)
        `shouldRespondWith` "{\"result\":10.0,\"statusMsg\":\"success\"}"
    
    it "multiplies two numbers" $ do
      post "/math" (jsonBody $ MathRequest "multiply" 5 4)
        `shouldRespondWith` "{\"result\":20.0,\"statusMsg\":\"success\"}"
    
    it "divides two numbers" $ do
      post "/math" (jsonBody $ MathRequest "divide" 10 2)
        `shouldRespondWith` "{\"result\":5.0,\"statusMsg\":\"success\"}"
    
    it "handles division by zero" $ do
      post "/math" (jsonBody $ MathRequest "divide" 10 0)
        `shouldRespondWith` 400
    
    it "returns 400 for invalid operation" $ do
      post "/math" (jsonBody $ MathRequest "invalid" 1 1)
        `shouldRespondWith` 400
