package com.geophy

import zio._
import scala.io.Source
import cats.Show
import cats.implicits._
import java.io.IOException

object HangMan extends ZIOAppDefault:

  extension [A](a: => A)
    def attempt: Task[A] = ZIO.attempt(a)
    def succeed: UIO[A] = ZIO.succeed(a)

  type WordList = List[String]

  def allWords: Task[WordList] =
    Source
      .fromResource("words.txt")
      .attempt
      .acquireReleaseWithAuto(_.getLines().toList.attempt)

  val minWordLength = 5
  val maxWordLength = 9

  def gameWords: Task[WordList] =
    allWords
      .map(
        _.filter(w => w.length >= minWordLength && w.length <= maxWordLength)
      )

  def randomWord(wordList: WordList) =
    Random.nextIntBounded(wordList.length).map(wordList(_))

  val selectRandomWord = gameWords flatMap randomWord

  case class Puzzle(
      word: String,
      filledInSoFar: List[Option[Char]],
      guessed: List[Char]
  )

  given Show[Puzzle] with
    def show(p: Puzzle): String =
      p.filledInSoFar
        .map {
          case None    => '_'
          case Some(c) => c
        }
        .mkString(" ") + s" Guessed so far: ${p.guessed.mkString("")}"

  def freshPuzzle(word: String): Puzzle =
    Puzzle(word, word.toList.map(_ => None), List.empty)

  def charInWord(p: Puzzle): Char => Boolean =
    p.word.toLowerCase.contains

  def alreadyGuessed(puzzle: Puzzle): Char => Boolean =
    puzzle.guessed.contains

  def fillInCharacter(p: Puzzle, c: Char): Puzzle =
    Puzzle(
      p.word,
      p.filledInSoFar.zip(p.word.toList).map {
        case (f, w) if w.toLower == c.toLower => Some(w)
        case (f, _)                           => f
      },
      c :: p.guessed
    )

  def handleGuess(
      p: Puzzle,
      c: Char,
      incorrectGuesses: Int
  ) =
    for
      _ <- Console.printLine(s"Your guess: $c")
      updatedState <- (
        charInWord(p)(c),
        alreadyGuessed(p)(c)
      ) match
        case (true, _) =>
          Console.printLine(
            "This character was in the word, filling in the word accordingly"
          ) *>
            (fillInCharacter(p, c), incorrectGuesses).succeed
        case (_, true) =>
          Console.printLine(
            "You've already guessed this character, try again"
          ) *>
            (p, incorrectGuesses).succeed
        case (_, _) =>
          Console.printLine("This character wasn't in the word, try again") *>
            (fillInCharacter(p, c), incorrectGuesses + 1).succeed
    yield updatedState

  def gameOver(p: Puzzle, incorrectGuesses: Int) =
    ZIO.ifZIO((incorrectGuesses == p.word.length).succeed)(
      Console.printLine(
        s"You've run out of guesses!\nThe word was: ${p.word}"
      ) *> true.succeed,
      false.succeed
    )

  def gameWin(p: Puzzle) =
    ZIO.ifZIO(p.filledInSoFar.forall(_.isDefined).succeed)(
      Console.printLine(
        s"You've guessed the word: ${p.word} !!!"
      ) *> true.succeed,
      false.succeed
    )

  def runGame(
      p: Puzzle,
      incorrectGuesses: Int
  ): ZIO[Any, Throwable, (Puzzle, Int)] =
    for
      shouldStop <- gameOver(p, incorrectGuesses).zipWith(gameWin(p))(_ || _)
      result <- ZIO.ifZIO(!shouldStop.succeed)(
        Console.printLine(s"Current puzzle is: ${p.show}") *>
          Console.printLine(
            s"You have ${p.word.length - incorrectGuesses} incorrect guesses left"
          ) *>
          Console.printLine("Please guess a letter:") *>
          Console.readLine.flatMap {
            case s if s.length == 1 =>
              handleGuess(p, s.head, incorrectGuesses) flatMap runGame
            case _ =>
              Console.printLine("Please enter a single character") *>
                runGame(p, incorrectGuesses)
          },
        (p, incorrectGuesses).succeed
      )
    yield result

  def run =
    for
      _ <- Console.printLine("Welcome to Hangman!")
      _ <- Console.printLine("Guess a letter to play!")
      word <- selectRandomWord
      puzzle = freshPuzzle(word)
      _ <- runGame(puzzle, 0)
    yield ExitCode.success
end HangMan
