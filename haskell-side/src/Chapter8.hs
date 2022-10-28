module Chapter8 where

-- factorial 0 = 1
-- factorial n = n * factorial (n -1)

-- andThen :: (a -> b) -> (b -> c) -> a -> c
-- andThen f g a = g (f a)

-- fibonacci :: Integer -> Integer
-- fibonacci 0 = 0
-- fibonacci 1 = 1
-- fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- dividedBy :: Integral a => a -> a -> (a, a)
-- dividedBy num den = go den 0 num
--   where
--     go den' quot rem
--       | rem < den' = (quot, rem)
--       | otherwise = go den' (quot + 1) (rem - den')

newtype SimpleCharacter = SimpleCharacter Int deriving (Show)

luke = SimpleCharacter 10

gameIntro = putStrLn "Welcome to Tomb of Horrors" >> gameLoop luke

gameLoop (SimpleCharacter hitPoints)
  | hitPoints <= 0 = putStrLn "Game Over, You died"
  | otherwise = do
    command <- getLine
    eventHandler hitPoints command

-- List of different damage types
characterEvents =
  [ ("Stab", "You got stabbed", -5),
    ("Fireball", "You got burned", -2),
    ("Blizzard", "You got frozen", -3),
    ("Spider", "A spider bit you", -4),
    ("Spikes", "You fell into a spike pit", -10),
    ("Goblin", "A goblin bit you", -1),
    ("Heal", "You found a healing potion", 5)
  ]

eventHandler :: Int -> String -> IO ()
eventHandler hitPoints "Hitpoints" = print hitPoints >> gameLoop (SimpleCharacter hitPoints)
eventHandler hitPoints command =
  let (msg, modifier) = findEvent command characterEvents
   in putStrLn msg >> gameLoop (SimpleCharacter (hitPoints + modifier))

findEvent command [] = ("Did not understand you", 0)
findEvent command ((cmd, msg, mdf) : xs) = if command == cmd then (msg, mdf) else findEvent command xs
