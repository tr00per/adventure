module Common where

type Name = String

class NamedObject a where
    getName :: a -> String
