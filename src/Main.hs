{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Config (loadConfig)
import Control.Monad.Reader (runReaderT)
import Server (server)

main :: IO ()
main = do
  config <- loadConfig
  runReaderT server config