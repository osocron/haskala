package com.geophy

trait Semigroup[A]:
  def combine(a1: A, a2: A): A

object Semigroup:
  
  given stringMonoid: Semigroup[String] with
    def combine(a1: String, a2: String): String = a1 + a2

  given intMonoid: Semigroup[Int] with
    def combine(a1: Int, a2: Int): Int = a1 + a2

  import scala.deriving.*
  import scala.compiletime._

  inline def summonAll[T <: Tuple]: List[Semigroup[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Semigroup[t]] :: summonAll[ts]

  def iterator[T](p: T) = p.asInstanceOf[Product].productIterator

  def toProduct(xs: List[Any]): Tuple = xs match {
    case Nil => EmptyTuple
    case (h :: t) => h *: toProduct(t)
  }

  def semigroupProduct[T](p: Mirror.ProductOf[T], elems: => List[Semigroup[_]]): Semigroup[T] =
    new Semigroup[T]:
      def combine(a1: T, a2: T): T =
        val zippedValues = iterator(a1).zip(iterator(a2)).zip(elems.iterator)
        val combined = zippedValues.map {
          case ((x, y), monoidInstance) => monoidInstance.asInstanceOf[Semigroup[Any]].combine(x, y)
        }
        p.fromProduct(toProduct(combined.toList))

  inline def semigroupSum[T](p: Mirror.SumOf[T], elems: => List[Semigroup[_]]): Semigroup[T] =
    new Semigroup[T]:
      def combine(a1: T, a2: T): T = 
        val ordA1 = p.ordinal(a1)
        val ordA2 = p.ordinal(a2)
        if (ordA1 == ordA2) then
          val instance = elems(ordA1).asInstanceOf[Semigroup[T]]
          instance.combine(a1, a2)
        else throw new Exception("Cannot combine different product types!")

  inline given derived[T](using m: Mirror.Of[T]): Semigroup[T] = 
    lazy val elemInstances: List[Semigroup[?]] = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T]     => semigroupSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => semigroupProduct(p, elemInstances)  

end Semigroup

enum LivingOrganism derives Semigroup:
  case Person(name: String, age: Int)
  case Cat(age: Int, remainingLives: Int, name: String)

object TestApp extends App:
  import LivingOrganism._
  val p1 = Person("Matt", 100)
  val p2 = Person("Vlad", 20)

  val livingOrgansimMonoid = summon[Semigroup[LivingOrganism]]
  val p3 = livingOrgansimMonoid.combine(p1, p2)

  println(p3)

  val cat1 = Cat(7, 3, "Whiskers")
  val cat2 = Cat(2, 9, "Fluffy")

  val p4 = livingOrgansimMonoid.combine(cat1, cat2)
  println(p4)