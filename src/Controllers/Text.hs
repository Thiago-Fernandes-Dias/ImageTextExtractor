{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Controllers.Text (controller) where

import Controllers (Controller)
import Data.String (IsString (fromString))
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (badRequest400, internalServerError500)
import Network.Wai.Parse (defaultParseRequestBodyOptions, fileContent)
import Services (Result (..))
import Web.Scotty (file, filesOpts, get, liftIO, post, status, text)

controller :: (String -> IO (Result String)) -> Controller
controller extract prefix = do
  get (fromString prefix) $ do
    file "static/index.html"

  post (fromString prefix) $ do
    filesOpts defaultParseRequestBodyOptions $ \_ fs -> do
      -- Assume we're processing only the first uploaded file for simplicity
      case fs of
        [] -> do
          status badRequest400
          text $ TL.pack "No file uploaded"
        ((_, fi) : _) -> do
          result <- liftIO (extract (fileContent fi))
          case result of
            Ok output -> text $ TL.pack output
            Error message -> do
              status internalServerError500
              text $ TL.pack message