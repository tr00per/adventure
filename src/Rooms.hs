module Rooms where

import           Common
import           Creatures (Creature)
import           Items     (Item)

data Room = Room {
            getNarrative :: String,
            getMonsters  :: [Creature],
            getItems     :: [Item],
            getExits     :: [RoomExit]
} deriving (Show)

data RoomExit = North Room | East Room | South Room | West Room

instance Show RoomExit where
    show (North _) = "North"
    show (East _)  = "East"
    show (South _) = "South"
    show (West _)  = "West"

instance NamedObject RoomExit where
    getName = show

follow :: RoomExit -> Room
follow (North r) = r
follow (East r)  = r
follow (South r) = r
follow (West r)  = r

showRoom :: Room -> String
showRoom (Room n ms is es) = unlines ["\n\nYou enter a room.", n, showEncounter ms, showTreasure is, showExits es]

showEncounter :: [Creature] -> String
showEncounter [] = "It's peaceful."
showEncounter ms = unlines $ "Enemy's ahead!":map show ms

showTreasure :: [Item] -> String
showTreasure [] = "There is nothing of intereset."
showTreasure is = unlines $ "You some items in the light of your torch.":map show is

showExits :: [RoomExit] -> String
showExits [] = "It's the end of your journey."
showExits es = unlines $ "You can go:":map show es

mkNarrativeChamber :: String -> [RoomExit] -> Room
mkNarrativeChamber plot = Room plot [] []

mkEmptyRoom :: [RoomExit] -> Room
mkEmptyRoom = mkNarrativeChamber "This is just an empty room"

mkEncounter :: [Creature] -> [RoomExit] -> Room
mkEncounter monsters = Room "This chamber is infested with monsters" monsters []

mkTreasure :: [Item] -> [RoomExit] -> Room
mkTreasure = Room "There's a lot of chests and other containers in this chamber" []
