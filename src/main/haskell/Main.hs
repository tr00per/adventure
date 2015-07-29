module Main where

import           Common       (NamedObject (..))
import           Creatures    (Player, mkPlayer, toCreature)
import           Dungeon      (DungeonState, dungeon, entry)
import           Exploration  (GameResult (..), explore)
import           Presentation (banner, gameover, prompt)
import           SavegameIO   (saveGameExists, loadAdventure, defaultLevel, saveAdventure)

main :: IO ()
main = do
    banner "Adventure"
    exists <- saveGameExists
    (player, level) <- if exists
                           then loadAdventure
                           else newAdventure
    if getName player == "show"
        then print level
        else startGame player level

startGame :: Player -> DungeonState -> IO ()
startGame player dstate = do
    putStrLn $ "Welcome, " ++ getName player ++ " the Adventurer!"
    result <- explore player dstate
    case result of
        SaveGame (p, ds) -> saveAdventure p ds
        _                -> gameover (show result)
