package com.geophy


// class Person {
//   def sayName(s: String): String = s"My name is $s"
// }

// class Student extends Person {

// }

// object Extensions:
//   extension (s: Student)
//     def sayName(s: String): s

// def sayName(p: Person): String = p.sayName

case class HitPoints(n: Int)

trait HitPointsT[A]:
  def calculateHitPoints(c: A): HitPoints

object HitPoints:
  implicit def characterHitPoints: HitPointsT[Character]  = new HitPointsT[Character] {
    def calculateHitPoints(c: Character): HitPoints = HitPoints(10)
  }

enum Weapon:
  val n: Int
  case Sword(n: Int)
  case Dagger(n: Int)

// class CanDoDamage a

// instance CanDoDamage Weapon

//  class CanDoDamage a => InflictsDamage a where
//    inflict :: a -> Character -> Character
//    getWeaponDamage :: a -> Int

trait CanDoDamage[A]
object CanDoDamage:
  implicit def canDoDamgeWeapon: CanDoDamage[Weapon] = new CanDoDamage[Weapon] {}

trait InflictsDamage[A](using CanDoDamage[A]):
  def inflict(a: A, char: Character): Character

// instance InflictsDamage Weapon where
//  inflict weapon (Character attr (HitPoints hp)) = Character attr (HitPoints (hp - (getWeaponDamage weapon)))   
object InflictsDamage:
  implicit def weaponInflicts: InflictsDamage[Weapon] = new InflictsDamage[Weapon] {
    def inflict(a: Weapon, char: Character): Character = char.copy(maxHitPoints = char.maxHitPoints - a.n)
  }

object Chapter6 extends App:
  import HitPoints._
  def calculateHP(c: Character)(implicit ev: HitPointsT[Character]) = ev.calculateHitPoints(c)
  val character: Character = ???
  calculateHP(character)