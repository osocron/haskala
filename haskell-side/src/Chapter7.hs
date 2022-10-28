module Chapter7 where

-- Do some examples of lambdas

myLabda = \x -> \y -> x + y + 3

-- Do some examples of HOF
myFun :: String -> String -> String
myFun f a = f ++ a

-- Do some examples of pattern matching
patternMatch :: Bool -> String
patternMatch True = "It is true"
patternMatch _ = "It is false"

patternMatch' :: Bool -> String
patternMatch' b =
  let myInnerVal = True
   in case myInnerVal of
        True -> "It is true"
        False -> "It is false"

guards :: Int -> String
guards x
  | x > 0 = "Greater than zero"
  | x < 0 = "Less than zero"
guards _ = "Default"

-- LOTR vs. Star Wars
-- Create datatypes for LOTR and Star Wars characters
data StarWars = DarthVader | LukeSkywalker | PrincessLeia | HanSolo | Chewie | Yoda | EmperorPalpatine

data Lotr = Frodo | Bilbo | Balrog | Morgoth | Gandalf | Gollum | Boromir | Gimli

data WhoWon = First | Second | None

genericWhoWon :: StarWars -> Lotr -> WhoWon
genericWhoWon a b = Second

-- Create a function that lets you see who wins between any two characters
-- Try to use pattern-matching + HOF
fight :: StarWars -> Lotr -> (StarWars -> Lotr -> WhoWon) -> String
fight a b f =
  let winner = f a b
   in case winner of
        First -> "First"
        Second -> "Second"
        None -> "None"
