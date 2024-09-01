{-# LANGUAGE OverloadedStrings #-}

module Controllers (Controller, image, serverError) where

import Data.String (IsString (fromString))
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types (internalServerError500)
import Web.Scotty (ActionM, ScottyM, file, setHeader, status, text)

type Controller = String -> ScottyM ()

-- | Serve an image file.
image :: String -> String -> ActionM ()
image path format = do
  file path
  setHeader "Content-Type" $ fromString format

serverError :: String -> ActionM ()
serverError message = do
  status internalServerError500
  text $ TL.pack message