{-# LANGUAGE ViewPatterns #-}
module Creatures where

import           Common
import           Items

import           Control.Monad.Writer (Writer, tell)

data Creature = Creature {
    creatureName :: Name,
    power        :: Int,
    armor        :: Int,
    health       :: Int
} deriving (Eq, Show, Read)

data BattleResult = CreatureWon | Draw | PlayerWon | NoEffect deriving (Eq)

newtype Player = Player { toCreature :: Creature }

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
        newPlayer <- enemy `attack` toCreature player
        newEnemy  <- toCreature player `attack` enemy
        if toCreature player == newPlayer && enemy == newEnemy
            then do tell ["Your attacks have no effect!"]
                    return (player, NoEffect)
            else battle (Player newPlayer) newEnemy

attack :: Creature -> Creature -> Writer [String] Creature
attacker `attack` defender = do
    let damage = power attacker - armor defender
    tell [getName attacker ++ " deals " ++ show damage ++ " damage to " ++ getName defender]
    return (reduceHealth defender damage)

reduceHealth :: Creature -> Int -> Creature
reduceHealth creature damage = creature { health = health creature - damage }

upgradePower, upgradeArmor, upgradeHealth :: Player -> Int -> Player
upgradePower player@(toCreature -> creature) v  =
    if power creature < v then Player (creature { power = v }) else player
upgradeArmor player@(toCreature -> creature) v  =
    if armor creature < v then Player (creature { armor = v }) else player
upgradeHealth player@(toCreature -> creature) v =
    if health creature < v then Player (creature { health = v }) else player

upgradePlayer :: Player -> Item -> Player
upgradePlayer p (Item _ (Weapon x)) = upgradePower p x
upgradePlayer p (Item _ (Armor x))  = upgradeArmor p x
upgradePlayer p (Item _ (Potion x)) = upgradeHealth p x

goblin :: Creature
goblin = Creature "Goblin" 1 0 3

woodenDoor :: Name -> Creature
woodenDoor name = Creature (name ++ " Door") 0 1 2
