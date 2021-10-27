module Parser where

import           Common     (NamedObject (..))
import           Creatures  (Creature)
import           Items      (Item)
import           Rooms      (Room (..), RoomExit)

import           Data.Char  (toLower)
import           Data.List  (find)
import           Data.Maybe (catMaybes, fromJust)

type LegalOptions = [String]

data Decision = Unknown | Save | Go RoomExit | Attack Creature | Get Item

legalOptions :: Room -> LegalOptions
legalOptions (Room _ ms is es fl) =
    catMaybes [allow "attack" ms, allow "get" is, allowGo es ms, allowFlee fl ms] ++ defaults
    where
        defaults = ["save"]

        allow cmd targets
            | null targets = Nothing
            | otherwise    = Just cmd

        allowGo [] _ = Nothing
        allowGo _ [] = Just "go"
        allowGo _ _  = Nothing

        allowFlee _        [] = Nothing
        allowFlee (Just _) _  = Just "flee"
        allowFlee _        _  = Nothing

parseDecision :: LegalOptions -> String -> Room -> Decision
parseDecision _     ""  _     = Unknown
parseDecision legal cmd room
    | command `notElem` legal = Unknown
    | command == "save"       = Save
    | command == "go"         = tryAction Go target (exits room)
    | command == "attack"     = tryAction Attack target (monsters room)
    | command == "get"        = tryAction Get target (items room)
    | command == "flee"       = Go (fromJust $ flee room)
    | otherwise               = Unknown
    where tokens  = words cmd
          command = head tokens
          target  = unwords (tail tokens)

tryAction :: NamedObject a => (a -> Decision) -> String -> [a] -> Decision
tryAction f target xs = maybe Unknown f $ find ((== lowTarget) . lower . getName) xs
    where lowTarget = lower target
          lower = map toLower
