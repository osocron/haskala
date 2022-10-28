package com.geophy.algebras

trait Monoid[A]:
  def mempty: A
  def mappend(a: A, b: A): A
  def mconcat(as: List[A]): A = as match
    case Nil => mempty
    case _   => as.reduce(mappend)

opaque type First[A] = Option[A]

object First:
  def apply[A](a: A): First[A] = Option(a)
  def none[A]: First[A] = Option.empty[A]

  extension [A](fa: First[A])
    def getOrElse(a: A): A = fa.getOrElse(a)
    def orElse(b: First[A]): First[A] = fa.orElse(b)

object TestMonoid extends App:

  given Monoid[Int] with
    def mempty: Int = 0
    def mappend(a: Int, b: Int) = a + b

  given [A]: Monoid[First[A]] with
    def mempty: First[A] = First.none
    def mappend(a: First[A], b: First[A]) = a orElse b

  given [A](using am: Monoid[A]): Monoid[Option[A]] with
    def mempty: Option[A] = None
    def mappend(a: Option[A], b: Option[A]): Option[A] =
      for
        aV <- a
        bV <- b
      yield am.mappend(aV, bV)

  println(summon[Monoid[Option[Int]]].mappend(Some(1), Some(2)))

  val optionList: List[Option[Int]] = List(Some(1), Some(2), Some(3))
  val reduced = summon[Monoid[Option[Int]]].mconcat(optionList)
  println(reduced)

end TestMonoid
