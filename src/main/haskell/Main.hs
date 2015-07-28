module Main where

import           Common       (NamedObject (..))
import           Creatures    (Player, mkPlayer, toCreature)
import           Dungeon      (DungeonState, dungeon, entry, loadLevel,
                               saveGame)
import           Exploration  (GameResult (..), explore)
import           Presentation (banner, gameover, prompt)

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

saveGameExists :: IO Bool
saveGameExists = return False

loadAdventure :: IO (Player, DungeonState)
loadAdventure = undefined

newAdventure :: IO (Player, DungeonState)
newAdventure = do
    level <- loadLevel
    name <- prompt "How do they call you, friend?" "Enter your name"
    return (mkPlayer name, entry level)

startGame :: Player -> DungeonState -> IO ()
startGame player dstate = do
    putStrLn $ "Welcome, " ++ getName player ++ " the Adventurer!"
    result <- explore player dstate
    case result of
        SaveGame (p, ds) -> saveGame (toCreature p) (dungeon ds)
        _                -> gameover (show result)
