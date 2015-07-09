module Items where

import           Common

data Item = Item {
                itemName :: Name,
                itemType :: ItemType
            } deriving (Eq)

data ItemType = Weapon Int |
                Armor Int |
                Potion Int
            deriving (Eq)

instance NamedObject Item where
    getName (Item name _) = name

showItem :: Item -> String
showItem (Item name kind) = name ++ " (" ++ showItemType kind ++ ")"

showItemType :: ItemType -> String
showItemType (Weapon _) = "Weapon"
showItemType (Armor _)  = "Armor"
showItemType (Potion _) = "Potion"

sword :: Item
sword = Item "Common Sword" (Weapon 2)
