{-# LANGUAGE PatternGuards #-}
module Main where

import           Common       (NamedObject (..))
import           Exploration  (GameResult (..), explore)
import           Presentation (banner, gameover)
import           SavegameIO   (GameStatus (..), saveGameExists, loadAdventure, newAdventure, saveAdventure)

main :: IO ()
main = do
    banner "Adventure"
    exists <- saveGameExists
    gameState <- if not exists then newAdventure else loadAdventure
    startGame gameState

startGame :: GameStatus -> IO ()
startGame status
    | (NewGame gameState)    <- status = go gameState
    | (GameLoaded gameState) <- status = go gameState
    | (GameError msg)        <- status = putStrLn $ "Error while starting the game: " ++ msg
    | otherwise                        = error "Wrong game status"
    where go (player, dstate) = do
              putStrLn $ "Welcome, " ++ getName player ++ " the Adventurer!"
              gameResult <- explore player dstate
              endGame gameResult

endGame :: GameResult -> IO ()
endGame (SaveGame (p, ds)) = do savedGame <- saveAdventure p ds
                                case savedGame of
                                  GameError msg -> putStrLn $ "Error while saving the game: " ++ msg
                                  GameSaved -> return ()
endGame result = gameover (show result)
