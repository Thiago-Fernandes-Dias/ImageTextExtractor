{-# LANGUAGE FlexibleContexts #-}

module Services.Edit (editImage, EditParams (..)) where

import Graphics.Image (Image, RGBA, VS (VS), X, Y, dims, readImageY, Array)
import Graphics.Image.IO (readImage, writeImage)
import Graphics.Image.Processing (Bilinear (..), Border (..), resize)
import Graphics.Image.Processing.Filter (applyFilter, gaussianBlur, laplacianFilter)
import Services (EditParams (..), Result (..), defaultEditParams)

editImage :: String -> String -> EditParams -> IO (Result ())
editImage input output params =
  let applyLaplacian = laplacian params
   in if applyLaplacian
        then do
          image <- readImageY VS input :: IO (Image VS Y Double)
          let r = resizeImage image (height params, width params)
              b = blurImage r (blur params)
              e = edges b
          writeImage output e
          return $ Ok ()
        else do
          imageOrError <- readImage input :: IO (Either String (Image VS RGBA Double))
          case imageOrError of
            Left s -> return (Error s)
            Right image -> do
              let resizedImage = resizeImage image (height params, width params)
                  blurredImage = blurImage resizedImage (blur params)
               in writeImage output blurredImage
              return $ Ok ()

resizeImage :: Array arr a cs => Image arr a cs -> (Maybe Int, Maybe Int) -> Image arr a cs
resizeImage image (Just h, Nothing) = let cw = snd $ dims image in resize Bilinear Edge (h, cw) image
resizeImage image (Nothing, Just w) = let ch = fst $ dims image in resize Bilinear Edge (w, ch) image
resizeImage image (Just h, Just w) = resize Bilinear Edge (h, w) image
resizeImage image (Nothing, Nothing) = image

blurImage :: (Array arr cs e, Array arr X e, Floating e, RealFrac e) => Image arr cs e -> Maybe e -> Image arr cs e
blurImage image Nothing = image
blurImage image (Just b) = applyFilter (gaussianBlur b) image

edges :: Image VS Y Double -> Image VS Y Double
edges = applyFilter (laplacianFilter Edge)