module Utils.Files where

import System.IO.Temp (emptySystemTempFile)

-- | Generate a temporary file path with the given prefix.
generateTempFilePath :: String -> IO FilePath
generateTempFilePath prefix = do
  emptySystemTempFile prefix
