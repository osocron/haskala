package com.geophy

import zio.Duration._
import java.util.ArrayList
import zio.Duration

class MyInt(n: Int) extends AnyVal

sealed trait Tree[+A]
case class Node[A](left: Tree[A], right: Tree[A], value: A) extends Tree[A]
case object Leaf extends Tree[Nothing]

object Folding extends App {

  // val result = List(1, 2, 3).foldRight(0)((item, acc) => {
  //   println(item); acc + item
  // })
  // val result2 = List(1, 2, 3).foldLeft(0)((acc, item) => {
  //   println(item); acc + item
  // })
  // println(result)
  // println(result)

  // enum Bool:
  //   case True
  //   case False

  val myTree: Tree[Int] = Node(Node(Leaf, Leaf, 7), Leaf, 15)

}

object AuxPattern extends App {

  trait Schedule[-Env, -In, +Out] {
    type State
    def initial: State
    def step(in: In, state: State): (State, Out, Descision)
  }

  sealed trait Descision
  case object Done extends Descision
  case class Continue(delay: Duration) extends Descision

  object Schedule {
    type WithState[-Env, -In, +Out, State0] = Schedule[Env, In, Out] {
      type State = State0
    }
  }

  trait DomainError

  trait InvalidPassword extends DomainError
  trait CouldNotDecrypt extends DomainError

  trait Union[-In] {
    type Out
    def apply(in: In): Out
  }

  object Union extends UnionLowPriority1 {
    type WithOut[In, Out0] = Union[In] { type Out = Out0 }

    implicit def UnionEitherRight[A]
        : Union.WithOut[Either[A, Either[A, A]], A] =
      new Union[Either[A, Either[A, A]]] {
        type Out = A
        def apply(in: Either[A, Either[A, A]]): A = in.fold(identity, _.merge)
      }
  }

  trait UnionLowPriority1 extends UnionLowPriority2 {
    implicit def UnionEither[A]: Union.WithOut[Either[A, A], A] =
      new Union[Either[A, A]] {
        type Out = A
        def apply(in: Either[A, A]): A = in.merge
      }
  }

  trait UnionLowPriority2 {}

  // def unify[In](in: In)(implicit union: Union[In]): union.Out = ???

  // val either1: Either[InvalidPassword, CouldNotDecrypt] = ???
  // val either2
  //     : Either[InvalidPassword, Either[CouldNotDecrypt, InvalidPassword]] = ???

  // val result1: DomainError = unify(either1)
  // val result2: DomainError = unify(either2)

}
