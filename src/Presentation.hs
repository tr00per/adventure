module Presentation where

import System.IO (hFlush, stdout)

banner :: String -> IO ()
banner msg = do
    putStrLn $ replicate len '-'
    putStrLn $ " " ++ msg
    putStrLn $ replicate len '-'
    where len = length msg + 2

prompt :: String -> String -> IO String
prompt msg status = do
    putStrLn msg
    putStr $ "(" ++ status ++ ") > "
    hFlush stdout
    getLine

gameover :: String -> IO ()
gameover msg = do
    putStrLn "G A M E   O V E R"
    putStrLn msg
