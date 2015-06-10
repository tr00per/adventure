module Dungeon where

import Creatures
import Items

import Data.Char (toLower)
import Control.Monad (unless)

data GameResult = Defeat | Victory deriving (Show, Eq)

data Directions = North | East | South | West

data Decision = Unknown | Go Room | Attack Creature | Get Item

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
    if null (getExits room)
        then return Victory
        else do
            printOptions room
            command <- getLine
            let decision = parseDecision command room
            decide player room decision

parseDecision :: String -> Room -> Decision
parseDecision "" _                       = Unknown
parseDecision cmd room@(Room _ ms is es)
    | command == "go"     = tryGo target es
    | command == "attack" = tryAttack target ms
    | command == "get"    = tryGet target is
    | otherwise           = Unknown
    where tokens  = (words . map toLower) cmd
          command = head tokens
          target  = unwords (tail tokens)

tryGo :: String -> [Room] -> Decision
tryGo target es = tryGo' $ lookup target [(show idx, e) | (idx, e) <- zip [1..] es]
    where tryGo' Nothing  = Unknown
          tryGo' (Just e) = Go e

tryAttack :: String -> [Creature] -> Decision
tryAttack target ms = tryAttack' $ lookup target [(Creatures.getName m, m) | m <- ms]
    where tryAttack' Nothing  = Unknown
          tryAttack' (Just m) = Attack m

tryGet :: String -> [Item] -> Decision
tryGet target is = tryGet' $ lookup target [(Items.getName i, i) | i <- is]
    where tryGet' Nothing  = Unknown
          tryGet' (Just i) = Get i

decide :: Player -> Room -> Decision -> IO GameResult
decide player room@(Room n ms is es) decision = case decision of
                                    Unknown         -> explore player room
                                    (Go target)     -> explore player target
                                    (Attack target) -> do
                                        let (newPlayer, battleResult) = battle player target
                                            ms'       = filter (/= target) ms
                                            newRoom   = Room n ms' is es
                                        if battleResult == PlayerWon
                                            then explore newPlayer newRoom
                                            else return Defeat
                                    (Get target)    -> do
                                        let newPlayer = upgradePlayer player target
                                            is'       = filter (/= target) is
                                            newRoom   = Room n ms is' es
                                        explore newPlayer newRoom

upgradePlayer :: Player -> Item -> Player
upgradePlayer p (Item _ (Weapon x)) = upgradePower p x
upgradePlayer p (Item _ (Armor x))  = upgradeArmor p x
upgradePlayer p (Item _ (Potion x)) = upgradeHealth p x

showRoom :: Room -> String
showRoom (Room n ms is es) = unlines ["\n\nYou enter a room.", n, showEncounter ms, showTreasure is, showExits es]

printOptions :: Room -> IO ()
printOptions (Room n ms is es) = do
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
