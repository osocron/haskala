module Chapter3 where

stringsAreFun = "Strings are  very fun!"

-- What is the type of a String? [V]
-- type [Char]
-- Create a list of chars [M]

as = ['f', 'o', 'o']

-- Concatenate some strings [A]
concatStrings = "Some strings" ++ "Other strings"

-- Reverse a string [AT]
result = reverse as

myReverse :: [Char] -> [Char]
myReverse word =
  inner word []
  where
    inner [] incReverse = incReverse
    inner decWord incReverse = inner (tail decWord) (head decWord : incReverse)

-- Create a list of numbers [V]
numbers = [4, 3, 2]

-- Concatenate lists of numbers [M]
-- Reverse a list of numbers [A]

myIntReverse :: [Int] -> [Int]
myIntReverse word =
  inner word []
  where
    inner [] incReverse = incReverse
    inner decWord incReverse = inner (tail decWord) (head decWord : incReverse)

-- def myFun[A](list: List[A]): List[A]
myGeneralReverse :: [a] -> [a]
myGeneralReverse word =
  inner word []
  where
    inner [] incReverse = incReverse
    inner decWord incReverse = inner (tail decWord) (head decWord : incReverse)

-- Print strings using do notation [AT]

greeting = ""

printSecond :: IO ()
printSecond = do
  putStrLn greeting

main :: IO ()
main = do
  putStrLn greeting
  printSecond
  where
    greeting = "Yarrrrr"

otherDoSomething :: IO ()
otherDoSomething = do
  putStrLn "Hello world"

-- Explain the error on page 79 of the book [AT]

-- Use take, drop, head, tail in some examples [AT]
-- What is !! and how to create an Excception in Haskell [M]

-- Create a function that replaces all occurrences of an specific word in a given string with another word [AT]
-- Use the above function to generate a small D&D story paragraph with the name of a character given to the function. [AT]

-- myReplace :: [Char] -> [Char] -> [Char] -> [Char]
-- myReplace originalText raplacement keyword = ???

-- matchSubset :: [Char] -> [Char] -> Int
-- matchSubse original subset = ???

splitBy :: [Char] -> Char -> [[Char]]
splitBy text char = inner text []
  where
    inner [] incList = incList
    inner decText incList = inner (dropWhile (/= char) text) (takeWhile (/= char) text : incList)
