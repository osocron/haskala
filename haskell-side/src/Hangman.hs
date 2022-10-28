module Hangman where

import Control.Monad (forever, when)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust)
import GHC.Weak (runFinalizerBatch)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

type WordList = [String]

type IncorrectGuesses = Int

data Puzzle = Puzzle String [Maybe Char] [Char]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/words.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = filter gameLength <$> allWords
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    intersperse ' ' (fmap renderPuzzleChar discovered)
      ++ " Guessed so far: "
      ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word notGuessed []
  where
    notGuessed = Nothing <$ word

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) = flip elem word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) = flip elem guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IncorrectGuesses -> IO (Puzzle, IncorrectGuesses)
handleGuess puzzle guess guesses = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return (puzzle, guesses)
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess, guesses)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess, guesses + 1)

gameOver :: Puzzle -> IncorrectGuesses -> IO ()
gameOver (Puzzle wordToGuess _ guessed) guesses =
  when (length wordToGuess == guesses) $
    do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  when (all isJust filledInSoFar) $
    do
      putStrLn "You win!"
      exitSuccess

runGame :: Puzzle -> IncorrectGuesses -> IO ()
runGame puzzle guesses = forever $ do
  gameOver puzzle guesses
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn $ "Number of incorrect guesses is: " ++ show guesses
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c guesses >>= uncurry runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle $ toLower <$> word
  runGame puzzle 0
