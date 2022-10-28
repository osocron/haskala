package com.geophy

import cats.effect.std.Queue.CircularBufferQueue

enum Mark:
  case Cross
  case Circle
  case Empty

case class Board(squares: List[List[Mark]]):
  def update(mark: Mark, position: Position): Board = this
  def doWeHaveAWinner(): Unit = println("yes")

case class Position(x: Int, y: Int)

enum TicTacToeActions:
  case MakeMove(mark: Mark, position: Position)
  case EvaluateWinner()

type Game = List[TicTacToeActions]

import TicTacToeActions._
def interpret(game: Game, board: Board): Unit = game match
  case Nil => println("Game over")
  case MakeMove(mark, position) :: rest =>
    interpret(rest, board.update(mark, position))
  case EvaluateWinner() :: rest =>
    board.doWeHaveAWinner()
    interpret(rest, board)

object TicTacToe extends App:
  import Mark._
  interpret(
    MakeMove(Cross, Position(0, 0)) ::
      MakeMove(Circle, Position(0, 1)) ::
      MakeMove(Cross, Position(0, 2)) ::
      EvaluateWinner() :: Nil,
    Board(
      List(
        List(Empty, Empty, Empty),
        List(Empty, Empty, Empty),
        List(Empty, Empty, Empty)
      )
    )
  )
