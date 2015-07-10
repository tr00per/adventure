module Parser where

import           Common
import           Creatures
import           Items
import           Rooms

import           Data.List  (find)
import           Data.Maybe (catMaybes, fromJust)

data Decision = Unknown | Go RoomExit | Attack Creature | Get Item

legalOptions :: Room -> [String]
legalOptions (Room _ ms is es fl) =
        catMaybes [allow "attack" ms, allow "get" is, allowGo es ms, allowFlee fl]
    where
        allow cmd targets
            | null targets = Nothing
            | otherwise    = Just cmd

        allowGo [] _ = Nothing
        allowGo _ [] = Just "go"
        allowGo _ _  = Nothing

        allowFlee (Just _) = Just "flee"
        allowFlee _        = Nothing

printOptions :: [String] -> IO ()
printOptions options = putStrLn $ unlines $ "Available actions:":options

parseDecision :: [String] -> String -> Room -> Decision
parseDecision _     ""  _                 = Unknown
parseDecision legal cmd room
    | command `notElem` legal = Unknown
    | command == "go"         = tryAction Go target (exits room)
    | command == "attack"     = tryAction Attack target (monsters room)
    | command == "get"        = tryAction Get target (items room)
    | command == "flee" && canFlee room = Go (fromJust $ flee room)
    | otherwise               = Unknown
    where tokens  = words cmd
          command = head tokens
          target  = unwords (tail tokens)

tryAction :: NamedObject a => (a -> Decision) -> String -> [a] -> Decision
tryAction f target xs = maybe Unknown f $ find ((== target) . getName) xs
