package com.geophy.handson

object Sudoku extends App:

  type SudokuBoard = Array[Array[Int]]
  type Row = Array[Int]

  val divLine = ("+" + "-" * 7) * 3 + "+\n"
  val renderDigit = (d: Int) => if d == 0 then " " else d.toString

  def renderRow(row: Row): String =
    row
      .grouped(3)
      .foldLeft("|")((acc, r) => s"$acc ${r.map(renderDigit).mkString(" ")} |")

  def renderSudoku(board: SudokuBoard): String =
    board
      .grouped(3)
      .foldLeft(divLine)((acc, group) =>
        s"$acc${group.map(renderRow).mkString("\n")}\n$divLine"
      )

  val sudoku = Array(
    Array(1, 0, 3, 0, 0, 0, 7, 0, 0),
    Array(0, 0, 7, 0, 8, 0, 0, 0, 5),
    Array(0, 0, 0, 7, 0, 3, 1, 0, 6),
    Array(0, 9, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 0, 2, 0, 0, 0, 0, 9, 0),
    Array(8, 0, 0, 0, 7, 1, 0, 0, 0),
    Array(0, 0, 0, 2, 0, 0, 8, 0, 0),
    Array(2, 0, 0, 0, 0, 0, 0, 0, 0),
    Array(0, 5, 0, 0, 4, 0, 6, 0, 3)
  )

  // println(renderSudoku(sudoku))

  def isValidSudoku(grid: SudokuBoard): Boolean = {
    !Range(0, 9).exists { i =>
      val row = Range(0, 9).map(grid(i)(_))
      val col = Range(0, 9).map(grid(_)(i))
      val square =
        Range(0, 9).map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3))
      row.filter(_ != 0).distinct.length + row.count(_ == 0) != row.length ||
      col.filter(_ != 0).distinct.length + col.count(_ == 0) != col.length ||
      square.filter(_ != 0).distinct.length + square.count(
        _ == 0
      ) != square.length
    }
  }

// println(isValidSudoku(sudoku))

end Sudoku
