{-# LANGUAGE ViewPatterns #-}
module Exploration where

import           Creatures
import           Dungeon
import           Parser
import           Presentation
import           Rooms

import           Control.Monad.Writer (runWriter)
import           Data.List            (delete)

data GameResult = Defeat | Victory deriving (Show, Eq)

follow :: RoomExit -> Dungeon -> DungeonState
follow (Exit _ pos) d@(rooms -> rss) = (rss !! position pos, d)

explore :: Player -> DungeonState -> IO GameResult
explore player dstate@(room, _) = do
    putStrLn $ showRoom room
    if isGameEnd room
        then return Victory
        else interaction player dstate

interaction :: Player -> DungeonState -> IO GameResult
interaction player dstate@(room, _) = do
    let legal = legalOptions room
    printOptions legal
    command <- prompt "What is your decision?" (showPlayer player)
    let decision = parseDecision legal command room
    decide player decision dstate

decide :: Player -> Decision -> DungeonState -> IO GameResult
decide player Unknown         dstate                 = do
    putStrLn "Unrecognized or malformed command."
    explore player dstate
decide player (Go target)     (_, dungeon)           = do
    putStrLn $ "You went " ++ showRoomExit target
    explore player (follow target dungeon)
decide player (Get target)    (room, dungeon)        = do
    let newPlayer = upgradePlayer player target
        is'       = delete target (items room)
        newRoom   = room { items = is' }
    explore newPlayer (newRoom, dungeon)
decide player (Attack target) dstate@(room, dungeon) = do
    let ((newPlayer, battleResult), steps) = runWriter (battle player target)
    putStrLn $ unlines steps
    case battleResult of
        PlayerWon -> let ms' = delete target (monsters room)
                         newRoom   = room { monsters = ms' }
                     in explore newPlayer (newRoom, dungeon)
        NoEffect  -> explore player dstate
        _         -> return Defeat
