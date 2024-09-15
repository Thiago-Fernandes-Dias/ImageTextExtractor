module Services where

data Result a = Ok a | Error String deriving (Show)

instance Functor Result where
  fmap f (Ok a) = Ok (f a)
  fmap _ (Error e) = Error e

instance Applicative Result where
  pure = Ok
  Ok f <*> Ok a = Ok (f a)
  Error e <*> _ = Error e
  _ <*> Error e = Error e

instance Monad Result where
  Ok a >>= f = f a
  Error e >>= _ = Error e

data EditParams = EditParams
  { blur :: Maybe Double,
    height :: Maybe Int,
    width :: Maybe Int,
    laplacian :: Bool
  }

defaultEditParams :: EditParams
defaultEditParams = EditParams Nothing Nothing Nothing False
