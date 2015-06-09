module Dungeon where

import Creatures
import Items

import Data.Char (toLower)

data GameResult = Defeat | Victory deriving (Show)

data Directions = North | East | South | West

data Decision = Unknown | Go String | Attack String | Get String

data Room = Room {
            getNarrative :: String,
            getMonsters  :: [Creature],
            getItems     :: [Item],
            getExits     :: [Room]
} deriving (Show)

type Dungeon = [Room]

explore :: Player -> Room -> IO GameResult
explore player room = do
    putStrLn $ showRoom room
    putStrLn $ showOptions room
    command <- getLine
    let decision = parseDecision command
    decide player room decision

parseDecision :: String -> Decision
parseDecision "" = Unknown
parseDecision cmd
    | command == "go"     = Go target
    | command == "attack" = Attack target
    | command == "get"    = Get target
    | otherwise           = Unknown
    where tokens  = (words . map toLower) cmd
          command = head tokens
          target  = unwords (tail tokens)

decide :: Player -> Room -> Decision -> IO GameResult
decide player room@(Room n ms is es) decision = case decision of
                                                Unknown -> explore player room

showRoom :: Room -> String
showRoom (Room n ms is es) = unlines ["You enter a room.", n, showEncounter ms, showTreasure is, showExits es]

showOptions :: Room -> String
showOptions (Room n ms is es) = "[Press Enter]"

showEncounter :: [Creature] -> String
showEncounter ms = if null ms
                      then "It's peaceful."
                      else unlines $ "Enemy's ahead!":map show ms

showTreasure :: [Item] -> String
showTreasure is = if null is
                     then "There is nothing of intereset."
                     else unlines $ "You some items in the light of your torch.":map show is

showExits es = if null es
                  then "It's the end of your journey."
                  else "You see " ++ (show $ length es) ++ " exit(s)."

mkNarrativeChamber :: String -> [Room] -> Room
mkNarrativeChamber plot exits = Room plot [] [] exits

mkEmptyRoom :: [Room] -> Room
mkEmptyRoom exits = mkNarrativeChamber "This is just an empty room" exits

mkEncounter :: [Creature] -> [Room] -> Room
mkEncounter monsters exits = Room "This chamber is infested with monsters" monsters [] exits

mkTreasure :: [Item] -> [Room] -> Room
mkTreasure items exits = Room "There's a lot of chests and other containers in this chamber" [] items exits

createDemoDungeon :: Dungeon
createDemoDungeon = [room1, room2a, room2b, room3, room4, room5] where
    room1  = mkNarrativeChamber "This is the crypt of the Demo Demon. You hope to find great treasures within it." [room2a]
    room2a = mkEncounter [goblin, goblin] [room3, room4]
    room2b = mkEmptyRoom [room3, room4]
    room3  = mkTreasure [sword] [room2b]
    room4  = mkEmptyRoom [room2b, room5]
    room5  = mkNarrativeChamber "You found the tomb of the Demo Demon, but it's empty. You go back to your home village and to your daily life." []
