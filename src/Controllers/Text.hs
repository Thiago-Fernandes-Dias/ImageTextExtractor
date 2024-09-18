{-# LANGUAGE OverloadedStrings #-}

module Controllers.Text (controller) where

import Controllers (Controller, serverError)
import Data.String (IsString (fromString))
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (badRequest400, internalServerError500)
import Network.Wai.Parse (defaultParseRequestBodyOptions, fileContent)
import Services (Result (..))
import Utils.Files (generateTempFilePath)
import Utils.Http (downloadFile)
import Web.Scotty (filesOpts, get, liftIO, post, queryParamMaybe, status, text)

-- | Create a controller for extracting text from an image.
controller :: (String -> IO (Result String)) -> Controller
controller extract prefix = do
  get (fromString prefix) $ do
    urlOrNothing <- queryParamMaybe "url"
    case urlOrNothing of
      Just url -> do
        temp <- liftIO (generateTempFilePath "haskeract-extract")
        successOrException <- liftIO $ downloadFile url temp
        case successOrException of
          Right _ -> do
            result <- liftIO $ extract temp
            case result of
              Ok output -> do
                text $ TL.pack output
              Error message -> do
                serverError message
          Left _ -> do
            serverError "Failed to download the file"
      Nothing -> do
        status badRequest400
        text $ TL.pack "No URL provided"

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