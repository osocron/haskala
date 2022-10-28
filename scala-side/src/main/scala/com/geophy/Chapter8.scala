package com.geophy

import cats.effect.IOApp
import cats.effect._
import cats._

// data SimpleCharacter = SimpleCharacter Int deriving (Show)
// luke = SimpleCharacter 10

case class SimpleCharacter (hitPoints: Int)
object GameLoop extends IOApp:

  // gameIntro = (putStrLn "Welcome to Tomb of Horrors") >> (gameLoop luke)
  val luke = SimpleCharacter(10)
  val gameIntro: IO[Unit] = IO.println("Welcome to Tomb of Horrors") >> gameLoop(luke)

  // gameLoop (SimpleCharacter hitPoints)
  // | hitPoints <= 0 = putStrLn "Game Over, You died"
  // | otherwise = do
  //   command <- getLine
  //   case command of
  //     "Stab" -> (putStrLn "You got stabbed") >> (gameLoop (SimpleCharacter (hitPoints - 5)))
  //     _ -> (putStrLn "I can't understand you") >> (gameLoop (SimpleCharacter hitPoints))

  def gameLoop(simpleCharacter: SimpleCharacter): IO[Unit] = {
    if simpleCharacter.hitPoints <= 0 then IO.println("You're dead.")
    else for {
      command <- IO.readLine
      _ <- handleCommand(command, simpleCharacter)
    } yield ()
  }

  def handleCommand(command: String, simpleCharacter: SimpleCharacter): IO[Unit] = command match
    case "Stab" => IO.println("You got stabbed") >> gameLoop(simpleCharacter.copy(hitPoints = simpleCharacter.hitPoints - 5))
    case _ => IO.println("I can't understand you") >> gameLoop(simpleCharacter)

  override def run(args: List[String]): IO[ExitCode] = 
    gameIntro.as(ExitCode.Success)
