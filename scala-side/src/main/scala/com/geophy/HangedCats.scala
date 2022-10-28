package com.geophy

import cats.effect._
import scala.io._
import cats.effect.std.Random
import cats._
import cats.implicits._
import cats.syntax._
import scala.language.postfixOps

object HangedCats extends IOApp:

  type WordList = List[String]

  val allWords: IO[WordList] =
    Resource
      .make(
        IO(Source.fromResource("words.txt"))
      )(r => IO(r.close))
      .use(r => IO(r.getLines.toList))

  val minWordLength = 5
  val maxWordLength = 9

  val gameWords = allWords.map(
    _.filter(w => w.length >= minWordLength && w.length <= maxWordLength)
  )

  val randomWord = for
    rnd <- Random.scalaUtilRandom[IO]
    words <- gameWords
    i <- rnd.nextIntBounded(words.length)
  yield words(i)

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
  ): IO[(Puzzle, Int)] =
    for
      _ <- IO.println(s"Your guess: $c")
      updatedState <- (
        charInWord(p)(c),
        alreadyGuessed(p)(c)
      ) match
        case (true, _) =>
          IO.println(
            "This character was in the word, filling in the word accordingly"
          ) *>
            IO.pure((fillInCharacter(p, c), incorrectGuesses))
        case (_, true) =>
          IO.println("You've already guessed this character, try again") *>
            IO.pure((p, incorrectGuesses))
        case (_, _) =>
          IO.println("This character wasn't in the word, try again") *>
            IO.pure((fillInCharacter(p, c), incorrectGuesses + 1))
    yield updatedState

  def gameOver(p: Puzzle, incorrectGuesses: Int): IO[Boolean] =
    Monad[IO].ifM(IO.pure(incorrectGuesses == p.word.length))(
      IO.println(
        s"You've run out of guesses!\nThe word was: ${p.word}"
      ) *> IO.pure(true),
      IO.pure(false)
    )

  def gameWin(p: Puzzle): IO[Boolean] =
    Monad[IO].ifM(IO.pure(p.filledInSoFar.forall(_.isDefined)))(
      IO.println(s"You've guessed the word: ${p.word} !!!") *> IO.pure(true),
      IO.pure(false)
    )

  def runGame(p: Puzzle, incorrectGuesses: Int): IO[(Puzzle, Int)] =
    for
      shouldStop <- Applicative[IO].map2(
        gameOver(p, incorrectGuesses),
        gameWin(p)
      )(_ || _)
      result <- Monad[IO].ifM((!shouldStop).pure)(
        IO.println(s"Current puzzle is: ${p.show}") *>
          IO.println(
            s"You have ${p.word.length - incorrectGuesses} incorrect guesses left"
          ) *>
          IO.println("Please guess a letter:") *>
          IO.readLine.flatMap {
            case s if s.length == 1 =>
              handleGuess(p, s.head, incorrectGuesses) >>= runGame
            case _ =>
              IO.println("Please enter a single character") *>
                runGame(p, incorrectGuesses)
          },
        IO.pure((p, incorrectGuesses))
      )
    yield result

  def run(args: List[String]): IO[ExitCode] =
    for
      _ <- IO.println("Welcome to Hangman!")
      _ <- IO.println("Guess a letter to play!")
      word <- randomWord
      puzzle = freshPuzzle(word)
      _ <- runGame(puzzle, 0)
    yield ExitCode.Success
end HangedCats
