module Chapter5 where

-- Model an RPG Character with Abilities, Attributes and Stats

newtype Strength = Strength Int deriving (Show)

newtype Dexterity = Dexterity Int deriving (Show)

newtype Wisdom = Wisdom Int deriving (Show)

newtype Charisma = Charisma Int deriving (Show)

newtype Constitution = Constitution Int deriving (Show)

newtype Intelligence = Intelligence Int deriving (Show)

data Attributes = Attributes Strength Dexterity Wisdom Charisma Constitution Intelligence deriving (Show)

newtype HitPoints = HitPoints Int deriving (Show)

data Character = Character Attributes HitPoints deriving (Show)
