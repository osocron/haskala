module Phones where

import GHC.Unicode (isUpper, toLower)

type Digit = Char

type Presses = Int

data PhoneKey = One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Star | Zero | Hashtag deriving (Show)

data PhoneLetter = PhoneLetter Char PhoneKey Presses deriving (Show)

newtype DaPhone = DaPhone [PhoneLetter] deriving (Show)

locateLetter :: DaPhone -> Char -> Maybe PhoneLetter
locateLetter (DaPhone []) _ = Nothing
locateLetter (DaPhone (h@(PhoneLetter c' _ _) : t)) c
  | c' == c = Just h
  | otherwise = locateLetter (DaPhone t) c

reverseTaps :: DaPhone -> Char -> [PhoneLetter]
reverseTaps phone c
  | isUpper c = case locateLetter phone (toLower c) of
    Just letter -> [PhoneLetter '*' Star 2, letter]
    Nothing -> []
  | otherwise = case locateLetter phone (toLower c) of
    Just letter -> [letter]
    Nothing -> []

cellPhonesDead :: DaPhone -> String -> [PhoneLetter]
cellPhonesDead phone = concatMap (reverseTaps phone)

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol OK. Have u ever tasted alcohol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "OK. Do u think I am pretty Lol",
    "Lol ya",
    "Just making sure rofl ur turn"
  ]

daPhone :: DaPhone
daPhone =
  DaPhone
    [ PhoneLetter '*' Star 2,
      PhoneLetter '1' One 1,
      PhoneLetter '2' Two 1,
      PhoneLetter '3' Three 1,
      PhoneLetter '4' Four 1,
      PhoneLetter '5' Five 1,
      PhoneLetter '6' Six 1,
      PhoneLetter '7' Seven 1,
      PhoneLetter '8' Eight 1,
      PhoneLetter '9' Nine 1,
      PhoneLetter '0' Zero 1,
      PhoneLetter 'a' Two 2,
      PhoneLetter 'b' Two 3,
      PhoneLetter 'c' Two 4,
      PhoneLetter 'd' Three 2,
      PhoneLetter 'e' Three 3,
      PhoneLetter 'f' Three 4,
      PhoneLetter 'g' Four 2,
      PhoneLetter 'h' Four 3,
      PhoneLetter 'i' Four 4,
      PhoneLetter 'j' Five 2,
      PhoneLetter 'k' Five 3,
      PhoneLetter 'l' Five 4,
      PhoneLetter 'm' Six 2,
      PhoneLetter 'n' Six 3,
      PhoneLetter 'o' Six 4,
      PhoneLetter 'p' Seven 2,
      PhoneLetter 'q' Seven 3,
      PhoneLetter 'r' Seven 4,
      PhoneLetter 's' Seven 5,
      PhoneLetter 't' Eight 2,
      PhoneLetter 'u' Eight 3,
      PhoneLetter 'v' Eight 4,
      PhoneLetter 'w' Nine 2,
      PhoneLetter 'x' Nine 3,
      PhoneLetter 'y' Nine 4,
      PhoneLetter 'z' Nine 5,
      PhoneLetter '#' Hashtag 1,
      PhoneLetter '^' Star 2,
      PhoneLetter '+' Zero 2,
      PhoneLetter '_' Zero 3,
      PhoneLetter '.' Hashtag 2,
      PhoneLetter ',' Hashtag 3,
      PhoneLetter ' ' Zero 1
    ]