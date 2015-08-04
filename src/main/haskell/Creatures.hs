{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}
module Creatures where

import           Common               (Name, NamedObject (..))
import           Items                (Item (..), ItemType (..))

import           Control.Monad.Writer (Writer, tell)

data Creature = Creature {
    creatureName :: Name,
    power        :: Int,
    armor        :: Int,
    health       :: Int
} deriving (Eq, Show, Read)

data BattleResult = CreatureWon | Draw | PlayerWon | NoEffect deriving (Eq)

newtype Player = Player { toCreature :: Creature } deriving (NamedObject, Show, Read)

instance NamedObject Creature where
    getName = creatureName

showCreature :: Creature -> String
showCreature (Creature n p a h) = n ++ " " ++ show p ++ "/" ++ show a ++ " (" ++ show h ++ ")"

showPlayer :: Player -> String
showPlayer = showCreature . toCreature

mkPlayer :: String -> Player
mkPlayer name = Player (Creature name 1 0 10)

battle :: Player -> Creature -> Writer [String] (Player, BattleResult)
battle player@(toCreature -> pc) enemy
    | health pc <= 0 && health enemy <= 0 = do
        tell ["You're both dead."]
        return (player, Draw)
    | health pc <= 0    = do
        tell ["You're dead."]
        return (player, CreatureWon)
    | health enemy <= 0 = do
        tell ["You won!"]
        return (player, PlayerWon)
    | otherwise         = do
        newPlayer <- enemy `attack` pc
        newEnemy  <- pc `attack` enemy
        if pc == newPlayer && enemy == newEnemy
            then do tell ["Your attacks have no effect!"]
                    return (player, NoEffect)
            else battle (Player newPlayer) newEnemy

attack :: Creature -> Creature -> Writer [String] Creature
attacker `attack` defender = do
    let damage = max 0 (power attacker - armor defender)
    tell [getName attacker ++ " deals " ++ show damage ++ " damage to " ++ getName defender]
    return (reduceHealth defender damage)

reduceHealth :: Creature -> Int -> Creature
reduceHealth creature damage = creature { health = newHealth }
    where newHealth = max 0 (health creature - damage)

class PlayerUpgrade u where
    upgradePlayer :: Player -> u -> Player

instance PlayerUpgrade Item where
    upgradePlayer player@(toCreature -> pc) (Item _ (Weapon newValue)) =
        if power pc < newValue then Player (pc { power = newValue }) else player
    upgradePlayer player@(toCreature -> pc) (Item _ (Armor newValue))  =
        if armor pc < newValue then Player (pc { armor = newValue }) else player
    upgradePlayer player@(toCreature -> pc) (Item _ (Potion newValue)) =
        if health pc < newValue then Player (pc { health = newValue }) else player

goblin :: Creature
goblin = Creature "Goblin" 1 0 3

woodenDoor :: Name -> Creature
woodenDoor name = Creature (name ++ " Door") 0 1 2
