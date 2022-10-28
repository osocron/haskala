package com.geophy

import com.geophy.Free.Isomorphism

object Free extends App:

  // Types are a set of possible values
  // Cardinality of types is the number of possible values
  // Int has 2 to the power of 32 possible values
  // String has infinite cardinality for all intents and purposes
  // When two types share the same cardinality, you get a one to one mapping
  // On -> True -> On
  // Off -> false -> Off
  // You can have nice round trips from one type to the other because they share the same cardinality
  // These types are called Isomorphic types, they contain the same amount of information, they are the same at the type level
  // Isomorphisms are useful to take ideas from our brains and put them into types, the symbolic mind can have isomorphic representations in our code

  // from . to = id[a]
  // to . from = id[b]
  final case class Isomorphism[A, B](to: A => B, from: B => A)

  object Isomorphism:
    // (A, B) <=> (B, A)

    implicit def iso[A, B]: Isomorphism[(A, B), (B, A)] =
      Isomorphism(_.swap, _.swap)
  end Isomorphism

  // Folds
  object Folds:
    // Fold is a function that takes a function and a seed and applies it to the seed
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
      as.foldLeft(z)(f)

    sealed trait Boolean:
      self =>
      // This is an ifThenElse!!!!!
      def fold[B](ifTrue: B)(ifFalse: B): B = self match {
        case Boolean.True  => ifTrue
        case Boolean.False => ifFalse
      }

      def &&(that: Boolean): Boolean = self.fold(that)(Boolean.False)
      def ||(that: Boolean): Boolean = self.fold(Boolean.True)(that)
      override def toString: String = self.fold("true")("false")
    end Boolean

    object Boolean:
      case object True extends Boolean
      case object False extends Boolean
    end Boolean

    // This is a Church encoding
    // A Church encoding if a Type represented as lambdas
    trait Coolean:
      // This is the fold function for Boolean. We call it apply for convenience in Scala
      def apply[A](ifTrue: A)(ifFalse: A): A

    object Coolean:
      val True = new Coolean:
        override def apply[A](ifTrue: A)(ifFalse: A): A = ifTrue

      val False = new Coolean:
        override def apply[A](ifTrue: A)(ifFalse: A): A = ifFalse

      def ifThenElse[A](cond: Coolean)(ifTrue: A)(ifFalse: A): A =
        cond(ifTrue)(ifFalse)
    end Coolean

    // Could we do the same for Option?
    sealed trait ChurchOption[+A]:
      def apply[B](ifSome: A => B)(ifNone: => B): B
    end ChurchOption

    object ChurchOption:
      val None: ChurchOption[Nothing] = new ChurchOption[Nothing]:
        override def apply[B](ifSome: Nothing => B)(ifNone: => B): B = ifNone

      def Some[A](a: A): ChurchOption[A] = new ChurchOption[A]:
        override def apply[B](ifSome: A => B)(ifNone: => B): B = ifSome(a)
    end ChurchOption

  end Folds

  sealed trait Nat:
    self =>
    def fold[B](zero: B)(succ: B => B): B = self match {
      case Nat.Zero    => zero
      case Nat.Succ(n) => succ(n.fold(zero)(succ))
    }
  end Nat

  object Nat:
    case object Zero extends Nat
    case class Succ(n: Nat) extends Nat

    val One = Succ(Zero)
    val Two = Succ(One)
    val Three = Succ(Two)
    val Four = Succ(Three)
  end Nat

// Data => Functions => Data => Functions => ... => Data
// Programs are ussually: Input => Output

end Free
