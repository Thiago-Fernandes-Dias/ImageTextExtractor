module Services.Rembg (removeBackground) where

import Services (Result (..))
import System.Exit (ExitCode (..))
import System.Process (readProcessWithExitCode)
import Web.Scotty (liftIO)

-- | Remove the background from an image.
removeBackground :: String -> String -> IO (Result ())
removeBackground input output = do
  (exitCode, stdout, stderr) <- liftIO $ readProcessWithExitCode "rembg" ["i", input, output] ""
  liftIO $ putStrLn stdout
  liftIO $ putStrLn stderr
  case exitCode of
    ExitSuccess -> return $ Ok ()
    ExitFailure _ -> return $ Error "Failed to remove background from the image"