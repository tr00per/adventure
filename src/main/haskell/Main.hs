module Main where

import           Creatures    (mkPlayer)
import           Dungeon      (Dungeon, entry, loadLevel)
import           Exploration  (explore)
import           Presentation (banner, gameover, prompt)

main :: IO ()
main = do
    banner "Adventure"
    dungeon <- loadLevel
    name <- prompt "How do they call you, friend?" "Enter your name"
    if name == "show"
        then print dungeon
        else newGame name dungeon

newGame :: String -> Dungeon -> IO ()
newGame name dungeon = do
    putStrLn $ "Welcome, " ++ name ++ " the Adventurer!"
    let player = mkPlayer name
    result <- explore player (entry dungeon)
    gameover (show result)
