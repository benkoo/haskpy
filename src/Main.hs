{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import System.Process (readProcess, readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (removeFile)
import qualified Control.Exception as E (catch, IOException, SomeException)
import Control.Exception (try)
import Text.Read (readMaybe)
import System.IO.Error (isDoesNotExistError)
import System.IO (hPutStrLn, stderr)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Web.Scotty (scotty, middleware, get, post, html, json, jsonData, pathParam, ActionM)
import qualified Web.Scotty as Scotty (status)
import Network.HTTP.Types (status400)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

-- Data types for our API
data MathRequest = MathRequest
    { operation :: String
    , x :: Double
    , y :: Double
    } deriving (Show, Generic)

instance FromJSON MathRequest
instance ToJSON MathRequest

data MathResponse = MathResponse
    { result :: Double
    , statusMsg :: String
    } deriving (Show, Generic)

instance ToJSON MathResponse

-- Our Haskell functions that we want to expose to Python
add :: Double -> Double -> Double
add a b = a + b

subtract' :: Double -> Double -> Double
subtract' a b = a - b

multiply :: Double -> Double -> Double
multiply a b = a * b

divide :: Double -> Double -> Maybe Double
divide _ 0 = Nothing
divide a b = Just (a / b)

-- Web server setup
startServer :: IO ()
startServer = scotty 3000 $ do
    middleware logStdoutDev
    
    get "/" $ do
        html $ mconcat
            [ "<h1>Haskell Web Service</h1>"
            , "<p>Available endpoints:</p>"
            , "<ul>"
            , "<li>POST /math - Perform math operations (add/subtract/multiply/divide)"
            , "<li>GET /fib/:n - Calculate nth Fibonacci number"
            , "</ul>"
            ]
    
    -- Math endpoint
    post "/math" $ do
        req <- jsonData :: ActionM MathRequest
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
    
    -- Fibonacci endpoint
    get "/fib/:n" $ do
        n <- pathParam "n"
        if n < 0
            then do
                Scotty.status status400
                json $ MathResponse 0 "n must be non-negative"
            else json $ MathResponse (fromIntegral $ fib n) "success"

-- Pure Haskell function for Fibonacci
fib :: Int -> Integer
fib n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- | Run a Python command and return its output
runPython :: String -> [String] -> IO String
runPython cmd args = readProcess "python3" (cmd:args) ""

-- | Run a Python command and get exit code and output
runPythonWithExit :: String -> [String] -> IO (ExitCode, String)
runPythonWithExit cmd args = do
    (exitCode, output, _) <- readProcessWithExitCode "python3" (cmd:args) ""
    return (exitCode, output)

-- | A simple example of calling Python from Haskell
pythonExample :: IO ()
pythonExample = do
    -- 1. Basic Python execution
    putStrLn "1. Basic Python execution:"
    _ <- runPython "-c" ["print('Hello from Python! 🐍')"]
    
    -- 2. Math operations with result parsing
    putStrLn "\n2. Math operations with result parsing:"
    let expr = "3.14159 * (5 ** 2)"  -- Area of a circle with r=5
    pyResult <- runPython "-c" ["print(" ++ expr ++ ")"]
    putStrLn $ "The area of a circle with radius 5 is: " ++ pyResult
    
    -- 3. Data exchange using JSON
    putStrLn "\n3. Data exchange using JSON:"
    let jsonExample = unlines
            [ "import json"
            , "data = {'name': 'Haskell', 'features': ['functional', 'lazy', 'static']}"
            , "print(json.dumps(data))"
            ]
    jsonResult <- runPython "-c" [jsonExample]
    putStrLn $ "Python sent us this JSON: " ++ jsonResult
    
    -- 4. Error handling
    putStrLn "\n4. Error handling:"
    (exitCode, errorOutput) <- runPythonWithExit "-c" ["1/0"]  -- This will cause a ZeroDivisionError
    case exitCode of
        ExitSuccess -> putStrLn "Unexpected success!"
        ExitFailure _ -> do
            putStrLn "Python encountered an error (as expected):"
            putStrLn $ "  " ++ errorOutput
    
    -- 5. Complex data processing with NumPy
    putStrLn "\n5. Data processing with NumPy (if installed):"
    let numpyExample = unlines
            [ "try:"
            , "    import numpy as np"
            , "    # Create and process some data"
            , "    data = np.array([1, 2, 3, 4, 5])"
            , "    mean = np.mean(data)"
            , "    std = np.std(data)"
            , "    print(f'Mean: {mean}, Standard deviation: {std}')"
            , "except ImportError:"
            , "    print('NumPy is not installed. Install with: pip install numpy')"
            ]
    numpyResult <- runPython "-c" [numpyExample]
    putStrLn $ "NumPy says: " ++ numpyResult
    
    -- 6. File I/O example
    putStrLn "\n6. File I/O example:"
    let fileExample = unlines
            [ "import tempfile"
            , "import os"
            , "# Create a temporary file"
            , "with tempfile.NamedTemporaryFile(delete=False) as f:"
            , "    f.write(b'Hello from Python\\n')"
            , "    f.write(b'This is a test file.')"
            , "    temp_path = f.name"
            , "print(temp_path)  # Print the path for Haskell to read"
            ]
    tempFilePath <- runPython "-c" [fileExample]
    let cleanPath = filter (/= '\n') tempFilePath
    
    -- Read the file content back in Haskell
    fileContent <- readFile cleanPath
    putStrLn $ "Python created a file with content: " ++ show fileContent
    
    -- Clean up the temporary file
    let removeIfExists file = E.catch (removeFile file) handleExists
            where handleExists e
                    | isDoesNotExistError e = return ()
                    | otherwise = hPutStrLn stderr ("Warning: Could not remove temporary file: " ++ show e)
    _ <- E.catch (removeIfExists cleanPath) (\e -> hPutStrLn stderr ("Warning: " ++ show (e :: E.IOException)) >> return ())
    
    -- 7. Interactive example with user input
    putStrLn "\n7. Interactive example (enter a number to calculate its factorial):"
    putStr "Enter a number (1-10): "
    input <- getLine
    case readMaybe input :: Maybe Int of
        Just n | n > 0 && n <= 10 -> do
            let factScript = unlines
                    [ "def fact(n):"
                    , "    return 1 if n == 1 else n * fact(n-1)"
                    , "print(str(fact(" ++ show n ++ ")))"
                    ]
            factResult <- runPython "-c" [factScript]
            putStrLn $ "The factorial of " ++ show n ++ " is: " ++ factResult
        _ -> putStrLn "Invalid input. Please enter a number between 1 and 10."

-- | Main function with error handling
main :: IO ()
main = do
    putStrLn "=== Advanced Python-Haskell Integration ===\n"
    
    -- Run the Python examples
    pyExampleResult <- try pythonExample :: IO (Either E.SomeException ())
    case pyExampleResult of
        Left err -> putStrLn $ "Error in Python examples: " ++ show err
        Right _ -> return ()
    
    putStrLn "\n=== Starting Haskell Web Service on http://localhost:3000 ==="
    putStrLn "You can now call these Haskell functions from Python:"
    putStrLn "- POST http://localhost:3000/math - Perform math operations"
    putStrLn "- GET  http://localhost:3000/fib/:n - Get nth Fibonacci number"
    putStrLn "\nExample Python code to call these endpoints is in python_client.py"
    
    -- Write example Python client code
    writeFile "python_client.py" $
        "import requests\n\n" ++
        "# Example 1: Call math endpoint\n" ++
        "response = requests.post('http://localhost:3000/math', \n" ++
        "    json={\"operation\": \"add\", \"x\": 10, \"y\": 20})\n" ++
        "print(\"Add: \", response.json())\n\n" ++
        "# Example 2: Call Fibonacci endpoint\n" ++
        "response = requests.get('http://localhost:3000/fib/10')\n" ++
        "print(\"10th Fibonacci number:\", response.json())\n"
    
    -- Start the web server
    startServer
