module SavegameIO where

import           Creatures              (Creature, Player, goblin, mkPlayer, woodenDoor)
import           Dungeon                (Dungeon, DungeonState, entry)
import           Items                  (smallPotion, sword)
import           Presentation           (prompt, promptWithDefault)
import           Rooms                  (Direction (..), Room, RoomExit (..),
                                         mkEncounter, mkNarrativeChamber,
                                         mkTreasure)

import           Control.Exception.Base (IOException, try)
import           Control.Monad          (liftM)
import           System.Directory       (doesFileExist, getPermissions,
                                         readable)
import           Text.Read              (readEither)

defaultLevel :: Dungeon
defaultLevel = [room0, room1, room2, room3, room4] where
    room0 = mkNarrativeChamber "This is the crypt of the Demo Demon. You hope to find great treasures within it." [Exit East 1]
    room1 = mkEncounter [goblin, goblin] [Exit West 0, Exit North 2, Exit South 3] Nothing
    room2 = mkTreasure [sword, smallPotion] [Exit South 1]
    room3 = mkEncounter [woodenDoor "Eastern"] [Exit North 1, Exit East 4] (Just $ Exit North 1)
    room4 = mkNarrativeChamber "You found the tomb of the Demo Demon, but it's empty. You go back to your home village and to your daily life." []


fromEitherIO :: (a -> IO c) -> (b -> IO c) -> Either a b -> IO c
fromEitherIO onLeft _       (Left x)  = onLeft x
fromEitherIO _      onRight (Right x) = onRight x

isReadable :: FilePath -> IO Bool
isReadable name = do
    exists <- doesFileExist name
    if exists
        then readable `liftM` getPermissions name
        else return False


tryIO :: IO a -> IO (Either IOException a)
tryIO = try

saveGameName :: FilePath
saveGameName = "savegame.txt"

saveGameExists :: IO Bool
saveGameExists = doesFileExist saveGameName

loadAdventure :: IO (Player, DungeonState)
loadAdventure = undefined

saveAdventure :: Player -> DungeonState -> IO ()
saveAdventure = undefined

newAdventure :: IO (Player, DungeonState)
newAdventure = do
    name <- prompt "How do they call you, friend?" "Enter your name"
    return (mkPlayer name, entry defaultLevel)

{-
loadLevel :: IO Dungeon
loadLevel = do
    levelName <- promptWithDefault "Choose a level" "new"
    canRead <- isReadable levelName
    if not canRead
        then do putStrLn "Level does not exist or is unreadable"
                loadLevel
        else loadDungeon levelName

loadWithDefault :: (Read a, Monoid a) => a -> String -> IO a
loadWithDefault defaultValue name
    | name == "new" = return defaultValue
    | otherwise     = tryIO (readFile name) >>= fromEitherIO (printErrorAndFail . show) parseResource

parseResource :: (Read a, Monoid a) => String -> IO a
parseResource contents = theResource `seq` fromEitherIO printErrorAndFail return theResource
    where theResource = readEither contents

printErrorAndFail :: (Monoid a) => String -> IO a
printErrorAndFail err = putStrLn err >> return mempty

loadDungeon :: String -> IO Dungeon
loadDungeon = loadWithDefault createDemoDungeon

loadGame :: FilePath -> IO (Creature, Dungeon)
loadGame file = return (pc, d)
    where pc = parseResource
          d  = parseResource .

loadAdventure :: IO (Player, DungeonState)
loadAdventure = loadGame "savegame.txt"

saveGame :: Creature -> Dungeon -> IO ()
saveGame pc d = print pc >> print d
-}
