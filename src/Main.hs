module Main where

import           Creatures    (mkPlayer)
import           Dungeon      (createDemoDungeon, entry)
import           Exploration  (explore)
import           Presentation (banner, gameover, prompt)

main :: IO ()
main = do
    banner "Adventure"
    name <- prompt "How do they call you, friend?" "Enter your name"
    putStrLn $ "Welcome, " ++ name ++ " the Adventurer!"
    let player = mkPlayer name
        dungeon = createDemoDungeon
    print dungeon
    result <- explore player (entry dungeon)
    gameover (show result)
