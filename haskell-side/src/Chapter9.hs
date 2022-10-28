module Chapter9 where

eftInt :: Int -> Int -> [Int]
eftInt from to
  | from > to = []
  | otherwise = from : eftInt (succ . toEnum $ from) to

-- List of different damage types
characterEvents :: [(String, Integer)]
characterEvents =
  [ ("You got burned", -2),
    ("You got frozen", -3),
    ("A spider bit you", -4),
    ("You fell into a spike pit", -10),
    ("A goblin bit you", -1),
    ("You found a healing potion", 5)
  ]
