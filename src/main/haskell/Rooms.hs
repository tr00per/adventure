{-# LANGUAGE ViewPatterns #-}
module Rooms where

import           Common    (NamedObject (..))
import           Creatures (Creature, showCreature)
import           Items     (Item, showItem)

data Room = Room {
    narrative :: String,
    monsters  :: [Creature],
    items     :: [Item],
    exits     :: [RoomExit],
    flee      :: Maybe RoomExit
} deriving (Show, Read)

data Direction = North | East | South | West deriving (Show, Read)

type Position = Int

data RoomExit = Exit Direction Position deriving (Show, Read)

showRoomExit :: RoomExit -> String
showRoomExit (Exit name _) = show name

instance NamedObject RoomExit where
    getName = showRoomExit

showRoom :: Room -> String
showRoom (Room n ms is es _) = unlines ["\n\nYou enter a room.", n, showEncounter ms, showTreasure is, showExits es]

isGameEnd :: Room -> Bool
isGameEnd = null . exits

canFlee :: Room -> Bool
canFlee (flee -> Just _) = True
canFlee _                = False

showEncounter :: [Creature] -> String
showEncounter [] = "It's peaceful."
showEncounter ms = unlines $ "Enemy's ahead!":map showCreature ms

showTreasure :: [Item] -> String
showTreasure [] = "There is nothing of intereset."
showTreasure is = unlines $ "You some items in the light of your torch.":map showItem is

showExits :: [RoomExit] -> String
showExits [] = "It's the end of your journey."
showExits es = unlines $ "You can go:":map showRoomExit es

mkNarrativeChamber :: String -> [RoomExit] -> Room
mkNarrativeChamber plot es = Room plot [] [] es Nothing

mkEmptyRoom :: [RoomExit] -> Room
mkEmptyRoom = mkNarrativeChamber "This is just an empty room"

mkEncounter :: [Creature] -> [RoomExit] -> Maybe RoomExit -> Room
mkEncounter ms = Room "This chamber is infested with monsters" ms []

mkTreasure :: [Item] -> [RoomExit] -> Room
mkTreasure is es = Room "There's a lot of chests and other containers in this chamber" [] is es Nothing
