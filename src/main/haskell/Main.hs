module Main where

import           Creatures    (mkPlayer)
import           Dungeon      (loadLevel, entry)
import           Exploration  (explore)
import           Presentation (banner, gameover, prompt)

main :: IO ()
main = do
    banner "Adventure"
    dungeon <- loadLevel
    name <- prompt "How do they call you, friend?" "Enter your name"
    putStrLn $ "Welcome, " ++ name ++ " the Adventurer!"
    let player = mkPlayer name
    print dungeon
    result <- explore player (entry dungeon)
    gameover (show result)
