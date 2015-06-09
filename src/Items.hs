module Items where

data Item = Weapon Int |
            Armor Int |
            Potion Int
    deriving (Show)

sword = Weapon 2
