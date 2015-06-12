module Rooms where

import Creatures (Creature)
import Items (Item)
import Common

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
