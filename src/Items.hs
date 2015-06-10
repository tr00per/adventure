module Items where

type Name = String

data Item = Item {
    getName :: String,
    getType :: ItemType
} deriving (Show, Eq)

data ItemType = Weapon Int |
                Armor Int |
                Potion Int
            deriving (Show, Eq)

sword :: Item
sword = Item "Common Sword" (Weapon 2)
