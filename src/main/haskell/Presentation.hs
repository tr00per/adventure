module Presentation where

import           System.IO (hFlush, stdout)

banner :: String -> IO ()
banner msg = do
    putStrLn line
    putStrLn $ " " ++ msg
    putStrLn line
    where len  = length msg + 2
          line = replicate len '-'

prompt :: String -> String -> IO String
prompt msg status = do
    putStrLn msg
    putStr $ "(" ++ status ++ ") > "
    hFlush stdout
    getLine

promptWithDefault :: String -> String -> IO String
promptWithDefault msg defaultValue = do
    result <- prompt msg ("default: " ++ defaultValue)
    if null result
        then return defaultValue
        else return result

gameover :: String -> IO ()
gameover msg = do
    putStrLn "G A M E   O V E R"
    putStrLn msg
