package com.geophy

import scala.quoted.Quotes
import scala.quoted._

case class MyMagicGen[T](v: T) {
  def print: Unit = println(v)
}

object Entrypoint:

  def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
    println(x)
    x

  inline def inspect(inline x: Any): Any = ${ inspectCode('x) }

  inline def logged[T](inline x: T): T = ${ loggedCode('x) }

  def loggedCode[T](x: Expr[T])(using Type[T], Quotes): Expr[T] = 
    println(x)
    x

  inline def autoCreate[T](inline x: T): MyMagicGen[T] = ${ autoClass('x) }

  def autoClass[T](x: Expr[T])(using Type[T], Quotes): Expr[MyMagicGen[T]] =
    import quotes.reflect._ 

    def traverseAST(term: Term): Term = term match
      case Inlined(t, xs, Apply(t1, xs1)) => 
        println(s"Found an Inlined Term! $t1")
        t1
      case _ => term

    val s = x.asTerm
    println(s.show(using Printer.TreeStructure))
    val found = traverseAST(x.asTerm)
    '{MyMagicGen($x)}


end Entrypoint
