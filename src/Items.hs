module Items where

import           Common

data Item = Item {
    itemName :: Name,
    itemType :: ItemType
} deriving (Eq, Show, Read)

data ItemType = Weapon Int |
                Armor Int |
                Potion Int
            deriving (Eq, Show, Read)

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

smallPotion :: Item
smallPotion = Item "Small Potion" (Potion 5)
