module Controllers (Controller) where

import Web.Scotty (ScottyM)

type Controller = String -> ScottyM ()
