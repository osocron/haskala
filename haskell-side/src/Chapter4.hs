module Chapter4 where

-- Discuss what are Types?
-- What is a Type Constructor?
-- What is a Data Constructor?
-- What is type-level and term-level?
-- Given the following data declaration:
data MyBool = MyFalse String | MyTrue

myBoolean :: MyBool
myBoolean = MyTrue

myBoolean2 :: MyBool
myBoolean2 = MyFalse "my_string"

-- Where is the Type Construcor?
-- Where is the Data Constructor?
-- How do we define type conjunction in Haskell? and
data Animal = Has String Int

data Human = Child | Adult

animal :: Animal
animal = Has "MyName" 22

myHuman :: Human
myHuman = Adult

data Property = WhateverIWant Int Int

-- How do we define type disjuntion in Haskell? or
-- What is the Scala equivalent of type conjunction?
-- case class Person(name: String, age: Int)
-- What is the Scala equivalent of type disjunction?
--

-- Define the Either data type in Haskell
data MyEither a b = MyLeft a | MyRight b deriving (Show)

myEither :: MyEither String String
myEither = MyRight "string"

-- Write the map function for Either (being right biased)

myMap :: MyEither a b -> (b -> c) -> MyEither a c
myMap (MyRight x) f = MyRight (f x)
myMap (MyLeft x) f = MyLeft x

-- Write the mapLeft function for Either
myMapLeft :: MyEither a b -> (a -> c) -> MyEither c b
myMapLeft (MyRight x) f = MyRight x
myMapLeft (MyLeft x) f = MyLeft (f x)

-- Write the isRight function
isRight :: MyEither a b -> Bool
isRight (MyRight x) = True
isRight (MyLeft x) = False

-- Write the isLeft function

-- Tuples and Lists??

tuples = (1, 2)

listOfTuples = [("string", 2), ("string", 4)]

myFold :: [(String, Int)] -> String
myFold [] = ""
myFold (h:t) = fst h ++ (myFold t)