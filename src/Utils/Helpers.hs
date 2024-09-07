module Utils.Helpers (readParam, readIntParam, readStringParam, readDoubleParam, readIntPairParam) where

import qualified Data.Text as T
import Text.Read (readMaybe)
import Web.Scotty (Param (..))

readParam :: String -> (String -> Maybe a) -> [Param] -> Maybe a
readParam _ _ [] = Nothing
readParam paramName f (p : ps)
  | T.unpack (fst p) == paramName = f $ T.unpack $ snd p
  | otherwise = readParam paramName f ps

readIntParam :: String -> [Param] -> Maybe Int
readIntParam paramName ps = readParam paramName readMaybe ps

readStringParam :: String -> [Param] -> Maybe String
readStringParam paramName ps = readParam paramName Just ps

readDoubleParam :: String -> [Param] -> Maybe Double
readDoubleParam paramName ps = readParam paramName readMaybe ps

readIntPairParam :: String -> [Param] -> Maybe (Int, Int)
readIntPairParam paramName ps = readParam paramName readMaybe ps
