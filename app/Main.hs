{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (badRequest400, internalServerError500)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Parse (defaultParseRequestBodyOptions, fileContent)
import System.Environment
import System.Exit (ExitCode (..))
import System.Process hiding (env)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Web.Scotty
import Prelude hiding (id)

-- | NB : the file paths where files are saved and looked up are relative, so make sure
-- to run this program from the root directory of the 'scotty' repo, or adjust the paths
-- accordingly.
main :: IO ()
main = do
  maybeEnv <- lookupEnv "ENV"
  let env = fromMaybe "PROD" maybeEnv

  scotty 3000 $ do
    middleware $ if env == "DEV" then logStdoutDev else logStdout

    get "/" $ do
      html $
        renderHtml $
          H.html $ do
            H.head $ do
              H.title "Haskerract"
              H.meta H.! charset "utf-8"
              H.meta H.! name "viewport" H.! content "width=device-width, initial-scale=1"
              H.script H.! src "https://cdn.tailwindcss.com" $ ""
            H.body H.! class_ "flex justify-center items-center h-full text-center" $ do
              H.div H.! class_ "bg-white shadow-md rounded-lg p-8 size-10/12 border-1 border-dash flex flex-col items-center justify-center cursor-pointer shadow-sm" $ do
                H.h1 H.! class_ "block text-gray-700 font-bold mb-2 text-2xl" $ "Haskerract"
                H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/" H.! class_ "block size-auto" H.! id "extract-form" $ do
                  H.label H.! class_ "block text-gray-700 text-sm font-bold mb-2" $ "Upload an image or drag it here" H.! for "image-input"
                  H.div H.! class_ "flex justify-center items-center w-full" $ do
                    H.input H.! type_ "file" H.! name "image" H.! class_ "flex rounded-md border border-input bg-background px-3 py-2 text-md shadow-sm transition-colors file:border-0 file:bg-transparent file:text-foreground file:text-md file:font-medium placeholder:text-muted-foreground focus-visible:outline-none focus-visible:ring-1 focus-visible:ring-ring" H.! id "image-input"
                    H.input H.! type_ "submit" H.! class_ "bg-gray-700 hover:bg-gray-900 text-white font-bold py-2 px-4 rounded-lg ml-2"

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
