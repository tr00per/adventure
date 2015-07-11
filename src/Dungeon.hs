module Dungeon where

import           Creatures
import           Items
import           Rooms

newtype Dungeon = Dungeon { rooms :: [Room] }

type DungeonState = (Room, Dungeon)

entry :: Dungeon -> DungeonState
entry d = (head $ rooms d, d)

upgradePlayer :: Player -> Item -> Player
upgradePlayer p (Item _ (Weapon x)) = upgradePower p x
upgradePlayer p (Item _ (Armor x))  = upgradeArmor p x
upgradePlayer p (Item _ (Potion x)) = upgradeHealth p x

createDemoDungeon :: Dungeon
createDemoDungeon = Dungeon [room0, room1, room2, room3, room4] where
    room0  = mkNarrativeChamber "This is the crypt of the Demo Demon. You hope to find great treasures within it." [Exit East (Position 1)]
    room1  = mkEncounter [goblin, goblin] [Exit West (Position 0), Exit North (Position 2), Exit South (Position 3)] Nothing
    room2  = mkTreasure [sword] [Exit South (Position 1)]
    room3  = mkEncounter [woodenDoor "Eastern"] [Exit North (Position 1), Exit East (Position 4)] (Just $ Exit North (Position 1))
    room4  = mkNarrativeChamber "You found the tomb of the Demo Demon, but it's empty. You go back to your home village and to your daily life." []
