{-# LANGUAGE OverloadedStrings #-}

module Controllers.Remove (controller) where

import Controllers (Controller, image, serverError)
import Data.String (IsString (fromString))
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (badRequest400)
import Network.Wai.Parse (defaultParseRequestBodyOptions, fileContent)
import Services (Result (..))
import Utils.Files (generateTempFilePath)
import Utils.Http (downloadFile)
import Web.Scotty (filesOpts, get, liftIO, post, queryParamMaybe, status, text)

controller :: (String -> String -> IO (Result ())) -> Controller
controller remove prefix = do
  get (fromString prefix) $ do
    urlOrNothing <- queryParamMaybe "url"
    case urlOrNothing of
      Just url -> do
        temp <- liftIO (generateTempFilePath "haskeract-remove")
        successOrException <- liftIO $ downloadFile url temp
        case successOrException of
          Right _ -> do
            let output = temp ++ "-result.png"
            result <- liftIO $ remove temp output
            case result of
              Ok _ -> do
                image output "image/png"
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
          temp <- liftIO $ generateTempFilePath "haskeract-remove"
          result <- liftIO $ remove (fileContent fi) temp
          case result of
            Ok _ -> do
              image temp "image/png"
            Error message -> do
              serverError message