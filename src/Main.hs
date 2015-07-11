module Main where

import           Creatures
import           Dungeon
import           Exploration
import           Presentation

main :: IO ()
main = do
    banner "Adventure"
    name <- prompt "How do they call you, friend?" "Enter your name"
    putStrLn $ "Welcome, " ++ name ++ " the Adventurer!"
    let player = mkPlayer name
        dungeon = createDemoDungeon
    print $ rooms dungeon
    result <- explore player (entry dungeon)
    gameover (show result)
