{-# LANGUAGE OverloadedStrings #-}

module Controllers.Edit (controller) where

import Controllers (Controller, image, serverError)
import Data.String (IsString (fromString))
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (badRequest400)
import Network.Wai.Parse (defaultParseRequestBodyOptions, fileContent)
import Services (Result (..), defaultEditParams)
import Services.Edit (EditParams (..))
import Utils.Files (generateTempFilePath)
import Utils.Helpers (readDoubleParam, readBoolParam, readIntParam)
import Web.Scotty (Param, filesOpts, liftIO, post, status, text)

controller :: (String -> String -> EditParams -> IO (Result ())) -> Controller
controller edit prefix = do
  Web.Scotty.post (fromString prefix) $ do
    Web.Scotty.filesOpts defaultParseRequestBodyOptions $ \ps fs -> do
      case fs of
        [] -> do
          Web.Scotty.status badRequest400
          Web.Scotty.text $ TL.pack "No file uploaded"
        ((_, fi) : _) -> do
          temp <- Web.Scotty.liftIO $ generateTempFilePath "haskeract-scale.png"
          result <- Web.Scotty.liftIO $ edit (fileContent fi) temp (parseEditParams ps)
          case result of
            Ok _ -> do
              image temp "image/png"
            Error message -> do
              serverError message

parseEditParams :: [Param] -> EditParams
parseEditParams [] = defaultEditParams
parseEditParams ps =
  EditParams
    (readDoubleParam "blur" ps)
    (readIntParam "height" ps)
    (readIntParam "width" ps)
    (readBoolParam "laplacian" ps)