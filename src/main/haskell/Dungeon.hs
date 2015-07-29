module Dungeon where

import           Rooms                  (Room, RoomExit (..))

type Dungeon = [Room]

type DungeonState = ([Room], [Room])

isEmpty :: Dungeon -> Bool
isEmpty = null

empty :: Dungeon
empty = []

entry :: Dungeon -> DungeonState
entry d = ([], d)

follow :: RoomExit -> Dungeon -> DungeonState
follow (Exit _ p) = splitAt p

currentRoom :: DungeonState -> Room
currentRoom (_, [])  = error "Degenerated dungeon"
currentRoom (_, r:_) = r

dungeon :: DungeonState -> Dungeon
dungeon (before, after) = before ++ after

updateCurrent :: Room -> DungeonState -> DungeonState
updateCurrent _   (_, [])           = error "Degenerated dungeon"
updateCurrent new (before, _:after) = (before, new:after)
