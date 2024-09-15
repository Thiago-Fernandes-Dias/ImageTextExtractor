module Utils.Helpers (readParam, readIntParam, readStringParam, readDoubleParam, readIntPairParam, readBoolParam) where

import Data.Maybe
import qualified Data.Text as T
import Text.Read (readMaybe)
import Web.Scotty (Param)

readParam :: String -> (String -> Maybe a) -> [Param] -> Maybe a
readParam _ _ [] = Nothing
readParam paramName f (p : ps)
  | T.unpack (fst p) == paramName = f $ T.unpack $ snd p
  | otherwise = readParam paramName f ps

readIntParam :: String -> [Param] -> Maybe Int
readIntParam paramName = readParam paramName readMaybe

readBoolParam :: String -> [Param] -> Bool
readBoolParam paramName ps =
  let a = readParam paramName (Just . stringToBool) ps
   in fromMaybe False a

readStringParam :: String -> [Param] -> Maybe String
readStringParam paramName = readParam paramName Just

readDoubleParam :: String -> [Param] -> Maybe Double
readDoubleParam paramName = readParam paramName readMaybe

readIntPairParam :: String -> [Param] -> Maybe (Int, Int)
readIntPairParam paramName = readParam paramName readMaybe

stringToBool :: String -> Bool
stringToBool "true" = True
stringToBool _ = False
