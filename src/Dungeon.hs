module Dungeon where

import           Creatures
import           Items
import           Parser
import           Presentation
import           Rooms

import           Control.Monad.Writer (runWriter)
import           Data.List            (delete)

newtype Dungeon = Dungeon { rooms :: [Room] }

data GameResult = Defeat | Victory deriving (Show, Eq)

entry :: Dungeon -> Room
entry = head . rooms

explore :: Player -> Room -> IO GameResult
explore player room = do
    putStrLn $ showRoom room
    if isGameEnd room
        then return Victory
        else interaction player room

interaction :: Player -> Room -> IO GameResult
interaction player room = do
    let legal = legalOptions room
    printOptions legal
    command <- prompt "What is your decision?" (showPlayer player)
    let decision = parseDecision legal command room
    decide player room decision

decide :: Player -> Room -> Decision -> IO GameResult
decide player room Unknown         = do
    putStrLn "Unrecognized or malformed command."
    explore player room
decide player _    (Go target)     = do
    putStrLn $ "You went " ++ showRoomExit target
    explore player (follow target)
decide player room (Attack target) = do
    let ((newPlayer, battleResult), steps) = runWriter (battle player target)
    putStrLn $ unlines steps
    case battleResult of
        PlayerWon -> let ms' = delete target (monsters room)
                         newRoom   = room { monsters = ms' }
                     in explore newPlayer newRoom
        NoEffect  -> explore player room
        _         -> return Defeat
decide player room (Get target)    = do
    let newPlayer = upgradePlayer player target
        is'       = delete target (items room)
        newRoom   = room { items = is' }
    explore newPlayer newRoom

upgradePlayer :: Player -> Item -> Player
upgradePlayer p (Item _ (Weapon x)) = upgradePower p x
upgradePlayer p (Item _ (Armor x))  = upgradeArmor p x
upgradePlayer p (Item _ (Potion x)) = upgradeHealth p x

createDemoDungeon :: Dungeon
createDemoDungeon = Dungeon [room1, room2a, room2b, room3, room4, room5] where
    room1  = mkNarrativeChamber "This is the crypt of the Demo Demon. You hope to find great treasures within it." [East room2a]
    room2a = mkEncounter [goblin, goblin] [North room3, South room4] Nothing
    room2b = mkEmptyRoom [North room3, South room4]
    room3  = mkTreasure [sword] [South room2b]
    room4  = mkEncounter [woodenDoor "Eastern"] [North room2b, East room5] (Just $ North room2b)
    room5  = mkNarrativeChamber "You found the tomb of the Demo Demon, but it's empty. You go back to your home village and to your daily life." []
