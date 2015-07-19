module Dungeon where

import           Common                 (fromEitherIO)
import           Creatures              (goblin, woodenDoor)
import           Items                  (smallPotion, sword)
import           Presentation           (promptWithDefault)
import           Rooms                  (Direction (..), Room, RoomExit (..),
                                         mkEncounter, mkNarrativeChamber,
                                         mkTreasure)

import           Control.Exception.Base (bracketOnError)
import           System.Directory       (doesFileExist, getPermissions,
                                         readable)
import           Text.Read              (readEither)

type Dungeon = [Room]

type DungeonState = ([Room], [Room])

entry :: Dungeon -> DungeonState
entry d = ([], d)

follow :: RoomExit -> Dungeon -> DungeonState
follow (Exit _ p) = splitAt p

currentRoom :: DungeonState -> Room
currentRoom (_, [])  = error "Degenerated dungeon"
currentRoom (_, r:_) = r

dungeon :: DungeonState -> Dungeon
dungeon (before, after) = before ++ after

updateCurrent :: Room -> DungeonState -> DungeonState
updateCurrent _   (_, [])           = error "Degenerated dungeon"
updateCurrent new (before, _:after) = (before, new:after)

createDemoDungeon :: Dungeon
createDemoDungeon = [room0, room1, room2, room3, room4] where
    room0  = mkNarrativeChamber "This is the crypt of the Demo Demon. You hope to find great treasures within it." [Exit East 1]
    room1  = mkEncounter [goblin, goblin] [Exit West 0, Exit North 2, Exit South 3] Nothing
    room2  = mkTreasure [sword, smallPotion] [Exit South 1]
    room3  = mkEncounter [woodenDoor "Eastern"] [Exit North 1, Exit East 4] (Just $ Exit North 1)
    room4  = mkNarrativeChamber "You found the tomb of the Demo Demon, but it's empty. You go back to your home village and to your daily life." []

loadLevel :: IO Dungeon
loadLevel = do
    levelName <- promptWithDefault "Choose a level" "demo_dungeon.txt"
    canRead <- isReadable levelName
    if not canRead
        then do putStrLn "Level does not exist or is unreadable"
                loadLevel
        else loadDungeon levelName

isReadable :: FilePath -> IO Bool
isReadable name = do
    exists <- doesFileExist name
    if exists
        then getPermissions name >>= \perm -> return $ readable perm
        else return exists

loadDungeonWithDefault :: Dungeon -> String -> IO Dungeon
loadDungeonWithDefault defaultDungeon name
    | name == "new" = return defaultDungeon
    | otherwise     = bracketOnError (readFile name) (\_ -> return defaultDungeon) $ \contents ->
        let theLevel = readEither contents in
        theLevel `seq` fromEitherIO theLevel (\err -> putStrLn err >> loadLevel) return

loadDungeon :: String -> IO Dungeon
loadDungeon = loadDungeonWithDefault createDemoDungeon
