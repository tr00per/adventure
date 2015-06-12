module Dungeon where

import Creatures
import Items
import Rooms
import Common
import Presentation

import Control.Monad (unless)
import Data.List (delete)

type Dungeon = [Room]

data GameResult = Defeat | Victory deriving (Show, Eq)

data Decision = Unknown | Go RoomExit | Attack Creature | Get Item

explore :: Player -> Room -> IO GameResult
explore player room = do
    putStrLn $ showRoom room
    if null (getExits room)
        then return Victory
        else do
            printOptions room
            command <- prompt "What is your decision?" (show player)
            let decision = parseDecision command room
            decide player room decision

parseDecision :: String -> Room -> Decision
parseDecision ""  _                 = Unknown
parseDecision cmd (Room _ ms is es)
    | command == "go"     = tryAction Go target es
    | command == "attack" = tryAction Attack target ms
    | command == "get"    = tryAction Get target is
    | otherwise           = Unknown
    where tokens  = words cmd
          command = head tokens
          target  = unwords (tail tokens)

tryAction :: NamedObject a => (a -> Decision) -> String -> [a] -> Decision
tryAction f target xs = tryAction' $ lookup target [(getName x, x) | x <- xs]
    where   tryAction' Nothing       = Unknown
            tryAction' (Just needle) = f needle

decide :: Player -> Room -> Decision -> IO GameResult
decide player room@(Room n ms is es) decision = case decision of
                                    Unknown         -> do
                                        putStrLn "Unrecognized or malformed command."
                                        explore player room
                                    (Go target)     -> do
                                        putStrLn $ "You went " ++ show target
                                        explore player (follow target)
                                    (Attack target) -> do
                                        let (newPlayer, battleResult) = battle player target
                                            ms'       = delete target ms
                                            newRoom   = Room n ms' is es
                                        if battleResult == PlayerWon
                                            then explore newPlayer newRoom
                                            else return Defeat
                                    (Get target)    -> do
                                        let newPlayer = upgradePlayer player target
                                            is'       = delete target is
                                            newRoom   = Room n ms is' es
                                        explore newPlayer newRoom

upgradePlayer :: Player -> Item -> Player
upgradePlayer p (Item _ (Weapon x)) = upgradePower p x
upgradePlayer p (Item _ (Armor x))  = upgradeArmor p x
upgradePlayer p (Item _ (Potion x)) = upgradeHealth p x

showRoom :: Room -> String
showRoom (Room n ms is es) = unlines ["\n\nYou enter a room.", n, showEncounter ms, showTreasure is, showExits es]

printOptions :: Room -> IO ()
printOptions (Room _ ms is es) = do
    putStrLn "Available actions:"
    unless (null ms) (putStrLn "attack")
    unless (null is) (putStrLn "get")
    unless (null es) (putStrLn "go")

showEncounter :: [Creature] -> String
showEncounter ms = if null ms
                      then "It's peaceful."
                      else unlines $ "Enemy's ahead!":map show ms

showTreasure :: [Item] -> String
showTreasure is = if null is
                     then "There is nothing of intereset."
                     else unlines $ "You some items in the light of your torch.":map show is

showExits :: [RoomExit] -> String
showExits es = if null es
                  then "It's the end of your journey."
                  else unlines $ "You can go:":map show es

mkNarrativeChamber :: String -> [RoomExit] -> Room
mkNarrativeChamber plot = Room plot [] []

mkEmptyRoom :: [RoomExit] -> Room
mkEmptyRoom = mkNarrativeChamber "This is just an empty room"

mkEncounter :: [Creature] -> [RoomExit] -> Room
mkEncounter monsters = Room "This chamber is infested with monsters" monsters []

mkTreasure :: [Item] -> [RoomExit] -> Room
mkTreasure = Room "There's a lot of chests and other containers in this chamber" []

createDemoDungeon :: Dungeon
createDemoDungeon = [room1, room2a, room2b, room3, room4, room5] where
    room1  = mkNarrativeChamber "This is the crypt of the Demo Demon. You hope to find great treasures within it." [East room2a]
    room2a = mkEncounter [goblin, goblin] [North room3, South room4]
    room2b = mkEmptyRoom [North room3, South room4]
    room3  = mkTreasure [sword] [South room2b]
    room4  = mkEmptyRoom [North room2b, East room5]
    room5  = mkNarrativeChamber "You found the tomb of the Demo Demon, but it's empty. You go back to your home village and to your daily life." []
