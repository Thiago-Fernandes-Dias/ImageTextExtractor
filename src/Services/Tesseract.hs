module Services.Tesseract (extractText) where

import Services (Result (..))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Web.Scotty (liftIO)

-- | Extract text from an image file.
extractText :: String -> IO (Result String)
extractText path = do
  (exitCode, stdout, _) <- liftIO $ readProcessWithExitCode "tesseract" [path, "stdout"] ""
  case exitCode of
    ExitSuccess -> return $ Ok stdout
    ExitFailure _ -> return $ Error "Failed to extract text from the image"