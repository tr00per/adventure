module Items where

import Common

data Item = Item Name ItemType deriving (Show, Eq)

data ItemType = Weapon Int |
                Armor Int |
                Potion Int
            deriving (Show, Eq)

sword :: Item
sword = Item "Common Sword" (Weapon 2)

instance NamedObject Item where
    getName (Item name _) = name
