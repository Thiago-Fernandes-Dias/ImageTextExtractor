{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Config (loadConfig, _env, _port)
import qualified Controllers.Remove as Remove
import qualified Controllers.Text as Text
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static
import Services.Rembg (removeBackground)
import Services.Tesseract (extractText)
import Web.Scotty

main :: IO ()
main = do
  config <- loadConfig

  scotty (_port config) $ do
    middleware $ if _env config == "DEV" then logStdoutDev else logStdout
    middleware $ staticPolicy (noDots >-> addBase "static")

    Text.controller extractText "/"
    Remove.controller removeBackground "/remove"