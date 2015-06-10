module Creatures where

data BattleResult = CreatureWon | Draw | PlayerWon deriving (Show, Eq)

data Creature = Creature {
                getName   :: String,
                getPower  :: Int,
                getArmor  :: Int,
                getHealth :: Int
} deriving (Show, Eq)

type Player = Creature

newPlayer :: String -> Player
newPlayer name = Creature name 1 0 10

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
