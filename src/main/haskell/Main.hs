module Main where

import           Common       (NamedObject (..))
import           Creatures    (Player)
import           Dungeon      (DungeonState)
import           Exploration  (GameResult (..), explore)
import           Presentation (banner, gameover)
import           SavegameIO   (saveGameExists, loadAdventure, newAdventure, saveAdventure)

main :: IO ()
main = do
    banner "Adventure"
    exists <- saveGameExists
    (player, level) <- if not exists
                           then newAdventure
                           else do loadedGame <- loadAdventure
                                   case loadedGame of
                                       Left _   -> undefined
                                       Right lg -> return lg
    if getName player == "show"
        then print level
        else startGame player level

startGame :: Player -> DungeonState -> IO ()
startGame player dstate = do
    putStrLn $ "Welcome, " ++ getName player ++ " the Adventurer!"
    result <- explore player dstate
    case result of
        SaveGame (p, ds) -> do savedGame <- saveAdventure p ds
                               case savedGame of
                                   Left _  -> undefined
                                   Right _ -> return ()
        _                -> gameover (show result)
