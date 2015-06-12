module Main where

import Dungeon
import Creatures
import Presentation

main :: IO ()
main = do
    banner "Adventure"
    name <- prompt "How do they call you, friend?" "Enter your name"
    putStrLn $ "Welcome, " ++ name ++ " the Adventurer!"
    let player = mkPlayer name
        dungeon = createDemoDungeon
    result <- runDungeon player dungeon
    gameover (show result)

runDungeon :: Player -> Dungeon -> IO GameResult
runDungeon player (room:_) = explore player room
runDungeon _      []       = return Defeat
