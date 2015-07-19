module Common where

type Name = String

class NamedObject a where
    getName :: a -> String

fromEitherIO :: Either a b -> (a -> IO c) -> (b -> IO c) -> IO c
fromEitherIO (Left x)  onLeft _       = onLeft x
fromEitherIO (Right x) _      onRight = onRight x
