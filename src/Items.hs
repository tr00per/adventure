module Items where

import           Common

data Item = Item Name ItemType deriving (Eq)

data ItemType = Weapon Int |
                Armor Int |
                Potion Int
            deriving (Eq)

sword :: Item
sword = Item "Common Sword" (Weapon 2)

instance NamedObject Item where
    getName (Item name _) = name

instance Show Item where
    show (Item name kind) = name ++ " (" ++ show kind ++ ")"

instance Show ItemType where
    show (Weapon _) = "Weapon"
    show (Armor _)  = "Armor"
    show (Potion _) = "Potion"
