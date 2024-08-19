{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (badRequest400, internalServerError500)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Network.Wai.Parse (defaultParseRequestBodyOptions, fileContent)
import System.Environment
import System.Exit (ExitCode (..))
import System.Process hiding (env)
import Web.Scotty
import Prelude hiding (id)

main :: IO ()
main = do
  maybeEnv <- lookupEnv "ENV"
  let env = fromMaybe "PROD" maybeEnv

  scotty 3000 $ do
    middleware $ if env == "DEV" then logStdoutDev else logStdout
    middleware $ staticPolicy (noDots >-> addBase "static")

    get "/" $ do
      file "static/index.html"

    post "/" $ do
      filesOpts defaultParseRequestBodyOptions $ \_ fs -> do
        -- Assume we're processing only the first uploaded file for simplicity
        case fs of
          [] -> do
            status badRequest400
            text $ TL.pack "No file uploaded"
          ((_, fi) : _) -> do
            let fpath = fileContent fi
            (exitCode, stdout, _) <- liftIO $ readProcessWithExitCode "tesseract" [fpath, "stdout"] ""
            case exitCode of
              ExitSuccess -> text $ TL.pack stdout
              ExitFailure _ -> do
                status internalServerError500
                text $ TL.pack "An error occurred while processing the image."
