module Creatures where

import           Common

data Creature = Creature Name Int Int Int deriving (Eq)

data BattleResult = CreatureWon | Draw | PlayerWon deriving (Eq)

type Player = Creature

instance NamedObject Creature where
    getName (Creature name _ _ _) = name

instance Show Creature where
    show (Creature name power armor health) = name ++ " " ++ show power ++ "/" ++ show armor ++ " (" ++ show health ++ ")"

getPower, getArmor, getHealth :: Creature -> Int
getPower (Creature _ power _ _)   = power
getArmor (Creature _ _ armor _)   = armor
getHealth (Creature _ _ _ health) = health

mkPlayer :: String -> Player
mkPlayer name = Creature name 1 0 10

goblin :: Creature
goblin = Creature "a goblin" 1 0 3

battle :: Player -> Creature -> (Player, BattleResult)
battle player@(getHealth -> 0) (getHealth -> 0) = (player, Draw)
battle player@(getHealth -> 0) _                = (player, CreatureWon)
battle player                  (getHealth -> 0) = (player, PlayerWon)
battle p                       c                = battle (c `attack` p) (p `attack` c)

attack :: Creature -> Creature -> Creature
attacker `attack` defender = reduceHealth defender (getPower attacker - getArmor defender)

reduceHealth :: Creature -> Int -> Creature
reduceHealth (Creature n p a h) damage = Creature n p a (h - damage)

upgradePower, upgradeArmor, upgradeHealth :: Player -> Int -> Player
upgradePower player@(Creature n p a h) v  = if p < v then Creature n v a h else player
upgradeArmor player@(Creature n p a h) v  = if a < v then Creature n p v h else player
upgradeHealth player@(Creature n p a h) v = if h < v then Creature n p a v else player
