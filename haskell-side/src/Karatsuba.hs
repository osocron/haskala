module Karatsuba where

karatsuba :: (Show a, Integral a) => a -> a -> a
karatsuba x y
  | max x y < ub = x * y
  | otherwise = z2 * base * base + (karatsuba (x1 + x0) (y1 + y0) - z2 - z0) * base + z0
  where
    base = 10 ^ ((length . show $ max x y) `div` 2)
    z2 = karatsuba x1 y1
    z0 = karatsuba x0 y0
    (x1, x0) = helper x
    (y1, y0) = helper y
    helper n = (n `div` base, n `mod` base)
    ub = 10000