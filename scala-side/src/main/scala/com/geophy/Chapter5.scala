package com.geophy

case class Dexterity(stat: Int)
case class Wisdom(stat: Int)
case class Charisma(stat: Int)
case class Constitution(stat: Int)
case class Intelligence(stat: Int)
case class Strength(stat: Int)

case class Attributes(strength: Strength, dexterity: Dexterity, wisdom: Wisdom, charisma: Charisma, constitution: Constitution, intelligence: Intelligence)

type MaxHitPoints = Int

case class Character(attributes: Attributes, maxHitPoints: MaxHitPoints)

enum Spell:
  case Fireball
  case EldritchBlast

type CurrentHitPoints = Int
type Damage = Int

object Chapter5 extends App:
  val myCh = Character(
    ???,
    100
  )
