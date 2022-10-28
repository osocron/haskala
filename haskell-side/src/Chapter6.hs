module Chapter6 where

import Chapter5

newtype Damage = Damage Int

data Spell = Fireball Int | EldritchBlast Int

data Weapon = Sword Int | Dagger Int

class CanDoDamage a

instance CanDoDamage Spell

instance CanDoDamage Weapon

class CanDoDamage a => InflictsDamage a where
  inflict :: a -> Character -> Character
  getWeaponDamage :: a -> Int

class RPGCharacter a

instance RPGCharacter Character

-- class Numberish a where
class RPGCharacter a => Hitpoints a where
  damage :: a -> Damage -> a

instance Hitpoints Character where
  damage (Character stats (HitPoints m)) (Damage n) = Character stats (HitPoints (m - n))

updateHitPoints :: Character -> HitPoints -> Character
updateHitPoints (Character stats _) = Character stats

matt = Character (Attributes (Strength 4) (Dexterity 1) (Wisdom 4) (Charisma (-2)) (Constitution 2) (Intelligence 3)) (HitPoints 15)

instance InflictsDamage Weapon where
  inflict weapon (Character attr (HitPoints hp)) = Character attr (HitPoints (hp - (getWeaponDamage weapon))) 

  getWeaponDamage (Dagger n) = n
  getWeaponDamage (Sword n) = n

spell = Fireball 20
dagger = Dagger 10
doDamageToMatt = inflict dagger matt