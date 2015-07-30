module SavegameIO where

import           Creatures              (Player, goblin, mkPlayer,
                                         woodenDoor)
import           Dungeon                (Dungeon, DungeonState, entry)
import           Items                  (smallPotion, sword)
import           Presentation           (prompt)
import           Rooms                  (Direction (..), RoomExit (..),
                                         mkEncounter, mkNarrativeChamber,
                                         mkTreasure)

import           Control.Exception.Base (IOException, bracket, try)
import           Control.Monad          (liftM)
import           System.Directory       (doesFileExist, getPermissions,
                                         readable)
import           System.IO              (IOMode (..), hClose, hPrint, hGetLine,
                                         openFile)
import           Text.Read              (readEither)

defaultLevel :: Dungeon
defaultLevel = [room0, room1, room2, room3, room4] where
    room0 = mkNarrativeChamber
                "This is the crypt of the Demo Demon. You hope to find great treasures within it."
                [Exit East 1]
    room1 = mkEncounter [goblin, goblin] [Exit West 0, Exit North 2, Exit South 3] Nothing
    room2 = mkTreasure [sword, smallPotion] [Exit South 1]
    room3 = mkEncounter [woodenDoor "Eastern"] [Exit North 1, Exit East 4] (Just $ Exit North 1)
    room4 = mkNarrativeChamber
                "You found the tomb of the Demo Demon, but it's empty.\
                \You go back to your home village and to your daily life." []

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
saveGameExists = isReadable saveGameName

both :: Show e => Either e a -> Either e b -> Either String (a, b)
both (Right player) (Right dstate) = Right (player, dstate)
both (Left err)     _              = Left ("Error while processing Player: " ++ show err)
both _              (Left err)     = Left ("Error while processing Dungeon: " ++ show err)

loadAdventure :: IO (Either String (Player, DungeonState))
loadAdventure = bracket (openFile saveGameName ReadMode) hClose loadData
    where loadData handle = do player <- readEither `liftM` hGetLine handle
                               dstate <- readEither `liftM` hGetLine handle
                               return (both player dstate)

saveAdventure :: Player -> DungeonState -> IO (Either String ((),()))
saveAdventure player dstate = bracket (openFile saveGameName WriteMode) hClose storeData
    where storeData handle = do playerWritten <- tryIO (hPrint handle player)
                                dstateWritten <- tryIO (hPrint handle dstate)
                                return (both playerWritten dstateWritten)

newAdventure :: IO (Player, DungeonState)
newAdventure = do
    name <- prompt "How do they call you, friend?" "Enter your name"
    return (mkPlayer name, entry defaultLevel)
