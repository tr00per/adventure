module Creatures where

data BattleResult = CreatureWon | Draw | PlayerWon deriving (Show) 

data Creature = Creature {
                getName   :: String,
                getPower  :: Int,
                getArmor  :: Int,
                getHealth :: Int
} deriving (Show)

type Player = Creature

newPlayer :: String -> Player
newPlayer name = Creature name 1 0 10

goblin :: Creature
goblin = Creature "a goblin" 1 0 3

battle :: Player -> Creature -> BattleResult
battle (getHealth -> 0) (getHealth -> 0) = Draw
battle (getHealth -> 0) _ = CreatureWon
battle _ (getHealth -> 0) = PlayerWon
battle p c = battle (c `attack` p) (p `attack` c)

attack :: Creature -> Creature -> Creature
attacker `attack` defender = reduceHealth defender (getPower attacker - getArmor defender)

reduceHealth :: Creature -> Int -> Creature
reduceHealth (Creature n p a h) damage = Creature n p a (h - damage)
