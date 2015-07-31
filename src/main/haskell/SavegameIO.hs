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

type GameState = (Player, DungeonState)

data GameStatus = GameError String | NewGame GameState | GameLoaded GameState | GameSaved

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

saveGameName :: FilePath
saveGameName = "savegame.txt"

saveGameExists :: IO Bool
saveGameExists = isReadable saveGameName

isReadable :: FilePath -> IO Bool
isReadable name = do
    exists <- doesFileExist name
    if exists
        then readable `liftM` getPermissions name
        else return False

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

gameLoaded :: Either String Player -> Either String DungeonState -> GameStatus
gameLoaded (Right player) (Right dstate) = GameLoaded (player, dstate)
gameLoaded (Left err)     _              = GameError ("Error while saving Player: " ++ err)
gameLoaded _              (Left err)     = GameError ("Error while saving Dungeon: " ++ err)

gameSaved :: Either IOError () -> Either IOError () -> GameStatus
gameSaved (Right ())     (Right ())     = GameSaved
gameSaved (Left err)     _              = GameError ("Error while loading Player: " ++ show err)
gameSaved _              (Left err)     = GameError ("Error while loading Dungeon: " ++ show err)

saveAdventure :: Player -> DungeonState -> IO GameStatus
saveAdventure player dstate = bracket (openFile saveGameName WriteMode) hClose storeData
    where storeData handle = do playerWritten <- tryIO (hPrint handle player)
                                dstateWritten <- tryIO (hPrint handle dstate)
                                return (gameSaved playerWritten dstateWritten)

loadAdventure :: IO GameStatus
loadAdventure = bracket (openFile saveGameName ReadMode) hClose loadData
    where loadData handle = do player <- readEither `liftM` hGetLine handle
                               dstate <- readEither `liftM` hGetLine handle
                               return (gameLoaded player dstate)

newAdventure :: IO GameStatus
newAdventure = do
    name <- prompt "How do they call you, friend?" "Enter your name"
    return $ NewGame (mkPlayer name, entry defaultLevel)
