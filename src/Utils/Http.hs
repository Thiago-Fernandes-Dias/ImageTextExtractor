{-# LANGUAGE OverloadedStrings #-}

module Utils.Http where

import Control.Exception (Exception (toException), SomeException)
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Simple
import System.IO (IOMode (WriteMode), withBinaryFile)

-- | Download a file from a URL and save it to a file.
downloadFile :: String -> FilePath -> IO (Either SomeException ())
downloadFile url filePath = do
  request <- parseRequest url
  response <- httpLBS request

  let status = getResponseStatusCode response
  case status of
    200 -> do
      let responseBody = getResponseBody response
      withBinaryFile filePath WriteMode $ \h ->
        L.hPut h responseBody
      return $ Right ()
    _ -> return $ Left $ toException $ userError "Failed to download the file"