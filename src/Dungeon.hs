module Dungeon where

import           Common
import           Creatures
import           Items
import           Presentation
import           Rooms

import           Data.List    (delete)
import           Data.Maybe   (catMaybes)

type Dungeon = [Room]

data GameResult = Defeat | Victory deriving (Show, Eq)

data Decision = Unknown | Go RoomExit | Attack Creature | Get Item

explore :: Player -> Room -> IO GameResult
explore player room = do
    putStrLn $ showRoom room
    if null (getExits room)
        then return Victory
        else interaction player room

interaction :: Player -> Room -> IO GameResult
interaction player room = do
    let legal = legalOptions room
    printOptions legal
    command <- prompt "What is your decision?" (show player)
    let decision = parseDecision legal command room
    decide player room decision

parseDecision :: [String] -> String -> Room -> Decision
parseDecision _     ""  _                 = Unknown
parseDecision legal cmd (Room _ ms is es)
    | command `notElem` legal = Unknown
    | command == "go"         = tryAction Go target es
    | command == "attack"     = tryAction Attack target ms
    | command == "get"        = tryAction Get target is
    | otherwise               = Unknown
    where tokens  = words cmd
          command = head tokens
          target  = unwords (tail tokens)

tryAction :: NamedObject a => (a -> Decision) -> String -> [a] -> Decision
tryAction f target xs = tryAction' $ lookup target [(getName x, x) | x <- xs]
    where   tryAction' Nothing       = Unknown
            tryAction' (Just needle) = f needle

decide :: Player -> Room -> Decision -> IO GameResult
decide player room              Unknown         = do
    putStrLn "Unrecognized or malformed command."
    explore player room
decide player _                 (Go target)     = do
    putStrLn $ "You went " ++ show target
    explore player (follow target)
decide player (Room n ms is es) (Attack target) = do
    let (newPlayer, battleResult) = battle player target
        ms'       = delete target ms
        newRoom   = Room n ms' is es
    if battleResult == PlayerWon
        then explore newPlayer newRoom
        else return Defeat
decide player (Room n ms is es) (Get target)    = do
    let newPlayer = upgradePlayer player target
        is'       = delete target is
        newRoom   = Room n ms is' es
    explore newPlayer newRoom

upgradePlayer :: Player -> Item -> Player
upgradePlayer p (Item _ (Weapon x)) = upgradePower p x
upgradePlayer p (Item _ (Armor x))  = upgradeArmor p x
upgradePlayer p (Item _ (Potion x)) = upgradeHealth p x

legalOptions :: Room -> [String]
legalOptions (Room _ ms is es) = catMaybes [allowAttack ms, allowGet is, allowGo es ms]
    where
        allowAttack [] = Nothing
        allowAttack _  = Just "attack"

        allowGet [] = Nothing
        allowGet _  = Just "get"

        allowGo [] _ = Nothing
        allowGo _ [] = Just "go"
        allowGo _ _  = Nothing

printOptions :: [String] -> IO ()
printOptions options = putStrLn $ "Available actions:" ++ unlines options

createDemoDungeon :: Dungeon
createDemoDungeon = [room1, room2a, room2b, room3, room4, room5] where
    room1  = mkNarrativeChamber "This is the crypt of the Demo Demon. You hope to find great treasures within it." [East room2a]
    room2a = mkEncounter [goblin, goblin] [North room3, South room4]
    room2b = mkEmptyRoom [North room3, South room4]
    room3  = mkTreasure [sword] [South room2b]
    room4  = mkEmptyRoom [North room2b, East room5]
    room5  = mkNarrativeChamber "You found the tomb of the Demo Demon, but it's empty. You go back to your home village and to your daily life." []
