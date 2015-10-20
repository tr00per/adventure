module Exploration where

import           Creatures            (BattleResult (..), Player, battle,
                                       showPlayer, upgradePlayer)
import           Dungeon              (DungeonState, currentRoom, dungeon,
                                       follow, updateCurrent)
import           Parser               (Decision (..), LegalOptions,
                                       legalOptions, parseDecision)
import           Presentation         (prompt)
import           Rooms                (Room (..), isGameEnd, showRoom,
                                       showRoomExit)

import           Control.Monad.Writer (runWriter)
import           Data.List            (delete)

data GameResult = Defeat | Victory | SaveGame (Player, DungeonState)

instance Show GameResult where
    show Defeat  = "Defeat."
    show Victory = "Victory!"
    show _       = error "Shouldn't get there"

explore :: Player -> DungeonState -> IO GameResult
explore player dstate = do
    let current = currentRoom dstate
    putStrLn $ showRoom current
    if isGameEnd current
        then return Victory
        else interaction player dstate

printOptions :: LegalOptions -> IO ()
printOptions options = putStrLn $ unlines $ "Available actions:":options

interaction :: Player -> DungeonState -> IO GameResult
interaction player dstate = do
    let current = currentRoom dstate
        legal   = legalOptions current
    printOptions legal
    command <- prompt "What is your decision?" (showPlayer player)
    let decision = parseDecision legal command current
    decide player decision dstate

decide :: Player -> Decision -> DungeonState -> IO GameResult
decide player Unknown         dstate = do
    putStrLn "Unrecognized or malformed command."
    explore player dstate
decide player Save            dstate = do
    putStrLn "Save and exit."
    return $ SaveGame (player, dstate)
decide player (Go target)     dstate = do
    putStrLn $ "You went " ++ showRoomExit target
    explore player (follow target $ dungeon dstate)
decide player (Get target)    dstate = do
    let current   = currentRoom dstate
        newPlayer = upgradePlayer target player
        is'       = delete target (items current)
        newRoom   = current { items = is' }
    explore newPlayer (updateCurrent newRoom dstate)
decide player (Attack target) dstate = do
    let current   = currentRoom dstate
        ((newPlayer, battleResult), steps) = runWriter (battle player target)
    putStrLn $ unlines steps
    case battleResult of
        PlayerWon -> let ms' = delete target (monsters current)
                         newRoom   = current { monsters = ms' }
                     in explore newPlayer (updateCurrent newRoom dstate)
        NoEffect  -> explore player dstate
        _         -> return Defeat
