module Services.Edit (editImage, EditParams (..), defaultEditParams) where

import Graphics.Image (Image, RGBA, VS, X, Y, convert)
import Graphics.Image.IO (Readable (..), readImage, writeImage)
import Graphics.Image.Processing (Bilinear (..), Border (..), resize)
import Graphics.Image.Processing.Filter (applyFilter, gaussianBlur, gaussianSmoothingFilter, laplacianFilter)
import Services (Result (..))

type Img = Image VS Y Double

data EditParams = EditParams
  { blur :: Maybe Double,
    newDimensions :: Maybe (Int, Int),
    laplacian :: Bool
  }

defaultEditParams = EditParams Nothing Nothing False

editImage :: String -> String -> EditParams -> IO (Result ())
editImage input output params = do
  imageOrError <- readImage input :: IO (Either String Img)
  case imageOrError of
    Left s -> return (Error s)
    Right image -> do
      let resizedImage = resizeImage image (newDimensions params)
          blurredImage = blurImage resizedImage (blur params)
          f = if laplacian params then edges blurredImage else blurredImage
       in writeImage output f
      return $ Ok ()

resizeImage :: Img -> Maybe (Int, Int) -> Img
resizeImage image Nothing = image
resizeImage image (Just new2DSize) = resize Bilinear Edge new2DSize image

blurImage :: Img -> Maybe Double -> Img
blurImage image Nothing = image
blurImage image (Just b) = applyFilter (gaussianBlur b) image

edges :: Img -> Img
edges = applyFilter (laplacianFilter Edge)