{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Controllers.Edit (controller) where

import Controllers (Controller, image, serverError)
import Data.String (IsString (fromString))
import qualified Data.Text.Lazy as TL
import Data.Text.Read (decimal)
import Network.HTTP.Types (badRequest400)
import Network.Wai.Parse (defaultParseRequestBodyOptions, fileContent)
import Services (EditParams (..), Result (..), defaultEditParams)
import Text.Read (readMaybe)
import Utils.Files (generateTempFilePath)
import Utils.Helpers (readDoubleParam, readIntPairParam)
import Web.Scotty (Param (..), filesOpts, get, liftIO, param, post, queryParamMaybe, status, text)

controller :: (String -> String -> EditParams -> IO (Result ())) -> Controller
controller scale prefix = do
  post (fromString prefix) $ do
    filesOpts defaultParseRequestBodyOptions $ \ps fs -> do
      case fs of
        [] -> do
          status badRequest400
          text $ TL.pack "No file uploaded"
        ((_, fi) : _) -> do
          temp <- liftIO $ generateTempFilePath "haskeract-scale.png"
          result <- liftIO $ scale (fileContent fi) temp (parseEditParams ps)
          case result of
            Ok _ -> do
              image temp "image/png"
            Error message -> do
              serverError message

parseEditParams :: [Param] -> EditParams
parseEditParams [] = defaultEditParams
parseEditParams ps = EditParams (readDoubleParam "blur" ps) (readIntPairParam "resize" ps) True