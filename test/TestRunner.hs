{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.Hspec.Runner (hspecWith, defaultConfig, Config(..))
import qualified APISpec
import qualified MathSpec
import System.Environment (withArgs, lookupEnv)
import System.Exit (exitWith, ExitCode(..))
import Control.Monad (when)

-- | Configuration for test execution
data TestConfig = TestConfig
  { configFailFast' :: Bool
  , configQuickMode :: Bool
  , configParallel :: Bool
  }

-- | Default test configuration
defaultTestConfig :: TestConfig
#ifdef QUICK
defaultTestConfig = TestConfig
  { configFailFast' = True
  , configQuickMode = True
  , configParallel = False
  }
#else
defaultTestConfig = TestConfig
  { configFailFast' = False
  , configQuickMode = False
  , configParallel = True
  }
#endif

-- | Main test specification
spec :: TestConfig -> Spec
spec testCfg = do
  when (configParallel testCfg) $ parallel $ do
    describe "Math Tests" $ MathSpec.spec
    describe "API Tests" $ APISpec.spec
  when (not $ configParallel testCfg) $ do
    describe "Math Tests (sequential)" $ MathSpec.spec
    describe "API Tests (sequential)" $ APISpec.spec

-- | Main entry point
main :: IO ()
main = do
  -- Check for QUICK_TEST environment variable
  quickMode <- maybe (pure $ configQuickMode defaultTestConfig) 
                    (pure . (== "1")) 
                =<< lookupEnv "QUICK_TEST"
  
  let cfg = defaultTestConfig { configQuickMode = quickMode }
  
  -- Configure Hspec
  let hspecCfg = defaultConfig
        { configFailFast = configFailFast' cfg
        , configQuickCheckMaxSuccess = Just $ if configQuickMode cfg then 10 else 100
        , configQuickCheckMaxSize = Just $ if configQuickMode cfg then 10 else 100
        , configQuickCheckSeed = Nothing
        }
  
  -- Run tests
  withArgs [] $ hspecWith hspecCfg $ spec cfg
