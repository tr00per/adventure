module Rooms where

import Creatures (Creature)
import Items (Item)
import Common

data RoomExit = North Room | East Room | South Room | West Room

data Room = Room {
            getNarrative :: String,
            getMonsters  :: [Creature],
            getItems     :: [Item],
            getExits     :: [RoomExit]
} deriving (Show)

instance Show RoomExit where
    show (North _) = "North"
    show (East _)  = "East"
    show (South _) = "South"
    show (West _)  = "West"

instance NamedObject RoomExit where
    getName r = show r

follow :: RoomExit -> Room
follow (North r) = r
follow (East r)  = r
follow (South r) = r
follor (West r)  = r
