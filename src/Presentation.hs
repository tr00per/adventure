module Presentation where

import System.IO (hFlush, stdout)

prompt :: String -> IO String
prompt msg = do
    putStrLn msg
    putStr ">"
    hFlush stdout
    getLine

gameover :: String -> IO ()
gameover msg = do
    putStrLn "G A M E   O V E R"
    putStrLn msg
