module Config (Config, _env, _port, loadConfig, defaultConfig) where

import qualified Configuration.Dotenv as Dotenv
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

-- | Configuration data type
data Config = Config
  { _port :: Int,
    _env :: String
  }

-- | Default configuration
defaultConfig :: Config
defaultConfig =
  Config
    { _port = 3000,
      _env = "DEV"
    }

-- | Load configuration from environment variables
loadConfig :: IO Config
loadConfig = do
  loadFile

  maybeEnv <- lookupEnv "ENV"
  maybePort <- lookupEnv "PORT"
  return
    Config
      { _port = fromMaybe (_port defaultConfig) (maybePort >>= safeRead),
        _env = fromMaybe (_env defaultConfig) maybeEnv
      }

-- | Load environment variables from .env files
loadFile :: IO ()
loadFile = do
  Dotenv.loadFile $
    Dotenv.Config
      { Dotenv.configPath = [".env.local", ".env"],
        Dotenv.configVerbose = False,
        Dotenv.configExamplePath = [],
        Dotenv.configOverride = True,
        Dotenv.configDryRun = False,
        Dotenv.allowDuplicates = True
      }

-- | Safely read an Int from a String, returning Nothing if the String is not a valid Int
safeRead :: String -> Maybe Int
safeRead s = case reads s of
  [(val, "")] -> Just val
  _ -> Nothing
