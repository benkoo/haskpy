{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module HaskPy.Main where

import qualified Prelude as P
import Prelude hiding (div)
import Control.Exception (try)
import qualified Control.Exception as E (SomeException)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Web.Scotty (scotty, middleware, get, post, html, json, jsonData, pathParam, ActionM)
import qualified Web.Scotty as Scotty (status)
import Network.HTTP.Types (status400)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import HaskPy.Math.Math (add, subtract', multiply)

-- | Request for math operations
data MathRequest = MathRequest
    { operation :: String  -- ^ Operation to perform: "add", "subtract", "multiply", or "divide"
    , x :: Int            -- ^ First operand
    , y :: Int            -- ^ Second operand
    } deriving (Generic)

deriving instance Show MathRequest

-- Explicit instances to avoid duplicate instance declarations
instance FromJSON MathRequest
instance ToJSON MathRequest

-- | Response from math operations
data MathResponse = MathResponse
    { result    :: Int     -- ^ Result of the operation
    , statusMsg :: String  -- ^ Status message
    } deriving (Generic)

deriving instance Show MathResponse

-- Explicit instance to avoid duplicate instance declarations
instance ToJSON MathResponse

-- Web server setup
startServer :: IO ()
startServer = scotty 3000 $ do
    middleware logStdoutDev
    
    get "/" $ do
        html $ mconcat
            [ "<h1>Haskell Web Service</h1>"
            , "<p>Available endpoints:</p>"
            , "<ul>"
            , "<li>GET /add/:x/:y - Add two numbers</li>"
            , "<li>GET /subtract/:x/:y - Subtract y from x</li>"
            , "<li>GET /multiply/:x/:y - Multiply two numbers</li>"
            , "<li>GET /divide/:x/:y - Divide x by y</li>"
            , "<li>POST /math - Perform math operation with JSON body</li>"
            , "</ul>"
            ]
    
    -- Math operations
    get "/add/:x/:y" $ do
        x' <- pathParam "x"
        y' <- pathParam "y"
        json $ MathResponse (add x' y') "success"
    
    get "/subtract/:x/:y" $ do
        x' <- pathParam "x"
        y' <- pathParam "y"
        json $ MathResponse (subtract' x' y') "success"
    
    get "/multiply/:x/:y" $ do
        x' <- pathParam "x"
        y' <- pathParam "y"
        json $ MathResponse (multiply x' y') "success"
    
    get "/divide/:x/:y" $ do
        x' <- pathParam "x" :: ActionM Int
        y' <- pathParam "y" :: ActionM Int
        if y' == 0
            then do
                Scotty.status status400
                json $ MathResponse 0 "Invalid operation or division by zero"
            else
                json $ MathResponse (x' `P.div` y') "success"
    
    -- JSON API endpoint
    post "/math" $ do
        req <- jsonData :: ActionM MathRequest
        let result = case operation req of
                "add"      -> Right $ fromIntegral (add (x req) (y req)) :: Either String Int
                "subtract" -> Right $ fromIntegral (subtract' (x req) (y req)) :: Either String Int
                "multiply" -> Right $ fromIntegral (multiply (x req) (y req)) :: Either String Int
                "divide"   -> case y req of
                                0 -> Left "Division by zero"
                                _ -> Right $ x req `P.div` y req
                _          -> Left "Invalid operation"
        
        case result of
            Left _ -> do
                Scotty.status status400
                -- Standardize error message for tests
                json $ MathResponse 0 "Invalid operation or division by zero"
            Right res ->
                json $ MathResponse res "success"

-- | Run a Python command and return its output
runPython :: String -> [String] -> IO (Either String String)
runPython cmd args = do
    (exitCode, stdout, errOutput) <- readProcessWithExitCode "python3" (cmd:args) ""
    return $ case exitCode of
        ExitSuccess -> Right stdout
        ExitFailure _ -> Left errOutput

-- Example usage of Python integration
pythonExample :: IO ()
pythonExample = do
    -- 1. Basic Python execution
    putStrLn "1. Basic Python execution:"
    _ <- runPython "-c" ["print('Hello from Python!')"]
    
    -- 2. Run a Python script
    putStrLn "\n2. Running a Python script:"
    result <- runPython "-c" ["import math; print(math.factorial(5))"]
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right out -> putStrLn $ "Factorial of 5 is: " ++ out
    
    -- 3. Error handling
    putStrLn "\n3. Error handling:"
    errResult <- runPython "-c" ["1/0"]
    case errResult of
        Left err -> putStrLn $ "Caught Python error: " ++ err
        Right out -> putStrLn out

-- | Main entry point
main :: IO ()
main = do
    putStrLn "=== Advanced Python-Haskell Integration ===\n"
    
    -- Run the Python examples
    pyExampleResult <- try pythonExample :: IO (Either E.SomeException ())
    case pyExampleResult of
        Left err -> putStrLn $ "Python example failed: " ++ show err
        _ -> return ()
    
    -- Start the web server
    putStrLn "\n=== Starting Web Server ==="
    putStrLn "Server is running on http://localhost:3000"
    startServer
