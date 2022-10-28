module Chapter2 where

z = 7

x = y ^ 2 -- 225

waxOn = r -- 1125
  where
    r = x * 5

y = z + 8 -- 17

triple x = x * 3

waxOff = triple

factorial 1 = 1
factorial n = n * factorial (n - 1)

rotations xs = take (length xs) (iterate (\(y : ys) -> ys ++ [y]) xs)

perms :: [a] -> [[a]]
perms = foldr (\x -> concatMap $ rotations . (x :)) [[]]