package com.geophy.handson

import cats.kernel.Order
import cats.implicits._
import java.awt.image.BufferedImage

object Chapter6 extends App:

  def findElement[A](xs: Array[A], elem: A)(using
      ord: Order[A]
  ): Boolean = {
    if xs.length == 1 then ord.eqv(xs(0), elem)
    else {
      val n = xs.length / 2
      val (leftArr, rightArr) = xs.splitAt(n)
      if ord.lteqv(elem, leftArr(n - 1)) then
        if ord.eqv(elem, leftArr(n - 1)) then true
        else findElement(leftArr, elem)
      else if ord.eqv(elem, rightArr(0)) then true
      else findElement(rightArr, elem)
    }
  }

  // println(findElement((0 to 50000).toArray, 5000))

  case class Trie(s: String) {
    case class Node(
        val hasValue: Boolean,
        val children: Map[Char, Node] = Map()
    )
    val root = {
      def go(str: String): Node = {
        if str.isEmpty then Node(false)
        else Node(hasValue = true, children = Map(str.head -> go(str.tail)))
      }
      go(s)
    }

  }

  val trie = Trie("hello, there")

  // trie.root.children.foreach(println)

  def search[T](start: T, graph: Map[T, Seq[T]]): Set[T] = {
    val seen = collection.mutable.Set(start)
    val queue = collection.mutable.ArrayDeque(start)
    while queue.nonEmpty do
      val current = queue.removeHead()
      for
        next <- graph(current)
        if !seen.contains(next)
      do
        seen.add(next)
        queue.append(next)
    seen.to(Set)
  }

  def searchPaths[T](start: T, graph: Map[T, Seq[T]]): Map[T, List[T]] = {
    val seen = collection.mutable.Map(start -> List(start))
    val queue = collection.mutable.ArrayDeque(start -> List(start))
    while queue.nonEmpty do
      val (current, path) = queue.removeHead()
      for
        next <- graph(current)
        if !seen.contains(next)
      do
        val newPath = next :: path
        seen(next) = newPath
        queue.append((next, newPath))
    seen.toMap
  }

  def depthFirstSearchPaths[T](
      start: T,
      graph: Map[T, Seq[T]]
  ): Map[T, List[T]] =
    val seen = collection.mutable.Map(start -> List(start))
    val pathLengths = collection.mutable.Map(start -> 0)
    val queue = collection.mutable.ArrayDeque((start, List(start), 0))
    while queue.nonEmpty do
      val (current, path, pathLength) = queue.removeLast()
      for
        next <- graph.getOrElse(current, Nil)
        if !seen
          .contains(next) && !pathLengths.get(next).exists(_ <= pathLength + 1)
      do
        val newPath = next :: path
        seen(next) = newPath
        pathLengths(next) = pathLength + 1
        queue.append((next, newPath, pathLength + 1))
    seen.toMap
  end depthFirstSearchPaths

  def shortestPath[T](start: T, dest: T, graph: Map[T, Seq[T]]): Seq[T] = {
    val shortestReversedPaths = depthFirstSearchPaths(start, graph)
    shortestReversedPaths(dest).reverse
  }

  // depthFirstSearchPaths(
  //   start = "a",
  //   graph = Map(
  //     "a" -> Seq("b", "c", "e"),
  //     "b" -> Seq("d"),
  //     "c" -> Seq("d"),
  //     "d" -> Seq(),
  //     "e" -> Seq()
  //   )
  // ).foreach(println)

  import Sudoku._

  def isComplete(sudoku: SudokuBoard): Boolean =
    sudoku.forall(_.forall(_ > 0))

  def nextAvailableMoves(sudoku: SudokuBoard): List[SudokuBoard] =
    val indexed = sudoku.zipWithIndex.map(t => (t._1.zipWithIndex, t._2))
    val index = indexed.find { case (arr, i) =>
      arr.exists(t => t._1 == 0)
    }
    index match
      case Some((arr, i)) =>
        val (_, j) = arr.find { case (e, _) => e == 0 }.get
        val oneToNine = Range(1, 10).toSet
        val row = Range(0, 9).map(sudoku(i)(_)).filter(_ != 0).toSet
        val col = Range(0, 9).map(sudoku(_)(j)).filter(_ != 0).toSet
        val gridGroups = Set(Set(0, 1, 2), Set(3, 4, 5), Set(6, 7, 8))
        val square =
          (for
            gI <- gridGroups.find(_.contains(i)).get
            gJ <- gridGroups.find(_.contains(j)).get
          yield sudoku(gI)(gJ)).filter(_ != 0)
        val available = (oneToNine.diff(row)) intersect (oneToNine.diff(
          col
        )) intersect (oneToNine.diff(square))
        available.toList.map(a => sudoku.updated(i, sudoku(i).updated(j, a)))
      case None => List()
  end nextAvailableMoves

  def isReallyValid(grid: SudokuBoard): Boolean =
    !Range(0, 9).exists { i =>
      val row = Range(0, 9).map(grid(i)(_))
      val col = Range(0, 9).map(grid(_)(i))
      val square =
        Range(0, 9).map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3))
      row.distinct.length != row.length ||
      col.distinct.length != col.length ||
      square.distinct.length != square.length
    }
  end isReallyValid

  def solveSudoku(sudoku: SudokuBoard): SudokuBoard =
    val seen = collection.mutable.Set(sudoku)
    val queue = collection.mutable.ArrayDeque(sudoku)
    var current = sudoku

    while !isComplete(current) && !isReallyValid(current) && !queue.isEmpty do
      current = queue.removeLast()
      for
        next <- nextAvailableMoves(current)
        if !seen.contains(next)
      do queue.append(next)

    nextAvailableMoves(current).headOption.getOrElse(current)
  end solveSudoku

  // println(renderSudoku(solveSudoku(sudoku)))

  def comparablePoints(
      x: Int,
      y: Int,
      raw: BufferedImage,
      currentColor: java.awt.Color,
      compareColors: (java.awt.Color, java.awt.Color) => Boolean,
      seen: scala.collection.mutable.Set[(Int, Int)]
  ): Set[(Int, Int)] =
    for
      xs <- Range(x - 1, x + 2).toSet
      ys <- Range(y - 1, y + 2).toSet
      if !seen.contains((xs, ys)) &&
        compareColors(currentColor, new java.awt.Color(raw.getRGB(xs, ys)))
    yield (xs, ys)

  def floodFill(
      src: String,
      dest: String,
      startX: Int,
      startY: Int,
      compareColors: (java.awt.Color, java.awt.Color) => Boolean,
      fillColor: java.awt.Color
  ): Unit =
    val raw =
      javax.imageio.ImageIO.read(new java.io.File(src))
    val seen = scala.collection.mutable.Set((startX, startY))
    val queue = collection.mutable.ArrayDeque((startX, startY))

    while queue.nonEmpty do
      val (x, y) = queue.removeHead()
      val currentColor = new java.awt.Color(raw.getRGB(x, y))
      raw.setRGB(x, y, fillColor.getRGB)
      for
        next <- comparablePoints(x, y, raw, currentColor, compareColors, seen)
      do
        seen.add(next)
        queue.append(next)

    javax.imageio.ImageIO.write(raw, "jpg", new java.io.File(dest))
    println("Done!")
  end floodFill

// floodFill(
//   src = "src/main/resources/Raw.jpg",
//   dest = "src/main/resources/Filled.jpg",
//   startX = 180,
//   startY = 90,
//   compareColors = { (a: java.awt.Color, b: java.awt.Color) =>
//     def sqrDiff(f: java.awt.Color => Int) = math.pow(f(a) - f(b), 2)
//     math.sqrt(
//       sqrDiff(_.getBlue) + sqrDiff(_.getGreen) + sqrDiff(_.getRed)
//     ) < 25
//   },
//   fillColor = java.awt.Color.BLACK
// )

  class MTrie[T]() {
    class Node(
        var hasValue: Boolean,
        var value: Option[T] = None,
        val children: collection.mutable.Map[Char, Node] =
          collection.mutable.Map()
    )
    val root = new Node(false)
    def add(s: String, v: T) =
      var current = root
      for (c, i) <- s.zipWithIndex do
        current = current.children.getOrElseUpdate(c, new Node(false))
      current.hasValue = true
      current.value = Some(v)

    def get(s: String): Option[T] =
      var current = Option(root)
      for c <- s if current.nonEmpty do current = current.get.children.get(c)
      current.flatMap(_.value)

    def contains(s: String): Boolean =
      var current = Option(root)
      for (c <- s if current.nonEmpty) current = current.get.children.get(c)
      current.exists(_.hasValue)

    def prefixesMatchingString0(s: String): Set[Int] =
      var current = Option(root)
      val output = Set.newBuilder[Int]
      for ((c, i) <- s.zipWithIndex if current.nonEmpty) do
        if (current.get.hasValue) output += i
        current = current.get.children.get(c)

      if (current.exists(_.hasValue)) output += s.length
      output.result()

    def prefixesMatchingString(s: String): Set[String] =
      prefixesMatchingString0(s).map(s.substring(0, _))

    def prefixesMatchingString1(s: String): Map[String, T] =
      prefixesMatchingString0(s)
        .map(s.substring(0, _))
        .foldLeft(Map.empty[String, T])((acc, next) =>
          acc + (next -> get(next).get)
        )

    def stringsMatchingPrefix(s: String): Set[String] =
      var current = Option(root)
      for (c <- s if current.nonEmpty)
        current = current.get.children.get(c) // initial walk
      if (current.isEmpty) Set()
      else
        val output = Set.newBuilder[String]
        def recurse(current: Node, path: List[Char]): Unit =
          if (current.hasValue) output += (s + path.reverse.mkString)
          for ((c, n) <- current.children) recurse(n, c :: path)

        recurse(current.get, Nil) // recursive walk
        output.result()

    def stringsMatchingPrefix0(s: String): Map[String, T] =
      stringsMatchingPrefix(s).foldLeft(Map.empty[String, T])((acc, next) =>
        acc + (next -> get(next).get)
      )
  }

  val t = new MTrie[Int]()
  t.add("mango", 1337); t.add("mandarin", 31337); t.add("map", 37);
  t.add("man", 7)
  println(t.get("man"))
  assert(t.get("mango") == Some(1337))
  assert(
    t.prefixesMatchingString1("mangosteen") == Map("man" -> 7, "mango" -> 1337)
  )
  assert(t.stringsMatchingPrefix0("mand") == Map("mandarin" -> 31337))

end Chapter6
