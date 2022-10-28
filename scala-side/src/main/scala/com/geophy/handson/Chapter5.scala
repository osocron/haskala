package com.geophy.handson

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import java.util.ArrayDeque

object Chapter5 extends App {

  def retry[T](max: Int, delay: Int = 0)(f: => T): T = {
    var tries = 0
    var result: Option[T] = None
    while (result == None) {
      try { result = Some(f) }
      catch {
        case e: Throwable =>
          tries += 1
          if (tries > max) throw e
          else {
            println(s"failed, retry #$tries")
            val currentDelay = (delay * scala.math.pow(2, tries)).toLong
            println(s"Waiting $currentDelay milliseconds before retrying")
            Thread.sleep(currentDelay)
          }
      }
    }
    result.get
  }

  // println(retry(5, 100)(throw new RuntimeException("Failed")))

  trait StrParser[T] {
    def parse(s: String): T
    def write(t: T): String
  }
  object StrParser {
    implicit object ParseInt extends StrParser[Int] {
      def parse(s: String) = s.toInt
      def write(t: Int) = s"$t"
    }
    implicit object ParseBoolean extends StrParser[Boolean] {
      def parse(s: String) = s.toBoolean
      def write(t: Boolean) = s"$t"
    }
    implicit object ParseDouble extends StrParser[Double] {
      def parse(s: String) = s.toDouble
      def write(t: Double) = s"$t"
    }
    implicit def ParseSeq[T](implicit p: StrParser[T]): StrParser[Seq[T]] =
      new StrParser[Seq[T]] {
        def parse(s: String): Seq[T] = {
          val jsonArray = """\[(.*)\]""".r
          s match {
            case jsonArray(elements) =>
              if (jsonArray.matches(elements)) {
                val elems = getNestedElements(elements)
                elems.map(p.parse)
              } else elements.split(",").toSeq.map(p.parse)
          }
        }

        def write(t: Seq[T]): String =
          t.map(p.write).mkString("[", ",", "]")
      }
    implicit def ParseTuple[T, V](implicit
        p1: StrParser[T],
        p2: StrParser[V]
    ): StrParser[(T, V)] =
      new StrParser[(T, V)] {
        def parse(s: String) = {
          val jsonArray = """\[(.*)\]""".r
          println(s)
          s match {
            case jsonArray(elements) =>
              if (jsonArray.matches(elements)) {
                val elems = getNestedElements(elements)
                (p1.parse(elems(0)), p2.parse(elems(1)))
              } else {
                val Array(t, v) = elements.split(",")
                (p1.parse(t), p2.parse(v))
              }
          }
        }

        def write(t: (T, V)): String =
          s"[${p1.write(t._1)},${p2.write(t._2)}]"
      }
  }

  def getNestedElements(s: String): List[String] = {
    val (_, results, _) = s.foldLeft((0, List.empty[String], ""))((acc, c) => {
      val (balance, elems, consumed) = acc
      val updatedBalance =
        if c == '[' then balance + 1
        else if c == ']' then balance - 1
        else balance
      val updatedConsumedStr =
        if updatedBalance == 0 && c == ',' then consumed else consumed + c
      if updatedBalance == 0 && c != ',' then {
        val updatedElems = elems :+ updatedConsumedStr
        (updatedBalance, updatedElems, "")
      } else (updatedBalance, elems, updatedConsumedStr)
    })
    results
  }

  def parseFromString[T](s: String)(implicit parser: StrParser[T]): T =
    parser.parse(s)

  def writeToString[T](t: T)(implicit parser: StrParser[T]): String =
    parser.write(t)

  println(parseFromString[Seq[Int]]("[1,2,3]"))
  println(parseFromString[Seq[(Int, Boolean)]]("[[1,true],[2,false]]"))
  println(
    parseFromString[Seq[(Seq[Int], Seq[(Boolean, Double)])]](
      "[[[1],[[true,0.5]]],[[2,3],[[false,1.5],[true,2.5]]]]"
    )
  )

  println(
    writeToString[Seq[Boolean]](Seq(true, false, true))
  ) // [true,false,true]
  println(
    writeToString[Seq[(Seq[Int], Seq[Boolean])]](
      Seq(
        (Seq(1), Seq(true)),
        (Seq(2, 3), Seq(false, true)),
        (Seq(4, 5, 6), Seq(false, true, false))
      )
    )
  )
  println(
    writeToString(
      Seq(
        (Seq(1), Seq((true, 0.5))),
        (Seq(2, 3), Seq((false, 1.5), (true, 2.5)))
      )
    )
  )
}
