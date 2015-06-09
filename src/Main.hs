module Main where

import Dungeon
import Creatures
import Presentation

main = do
    putStrLn "Adventure"
    name <- prompt "What is your name?"
    putStrLn $ "Welcome, " ++ name ++ " the Adventurer!"
    let player = newPlayer name
    putStrLn $ "Created: " ++ show player
    let monster = goblin
        whoWon = battle player monster
    putStrLn $ getName player ++ " vs. " ++ getName monster ++ ": " ++ show whoWon
    let dungeon = createDemoDungeon
    result <- runDungeon player dungeon
    gameover (show result)

runDungeon :: Player -> Dungeon -> IO GameResult
runDungeon player (room:_) = explore player room
