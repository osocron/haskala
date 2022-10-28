module CountInversions where

import Data.Bifunctor (Bifunctor (bimap, second))

mergeCount :: Ord a => [a] -> [a] -> (Int, [a])
mergeCount [] [] = (0, [])
mergeCount as [] = (0, as)
mergeCount [] bs = (0, bs)
mergeCount (a : as) (b : bs)
  | a < b =
    let merged = mergeCount as (b : bs)
     in second (a :) merged
  | otherwise =
    let merged = mergeCount (a : as) bs
     in bimap (length (a : as) +) (b :) merged

applyBoth :: (a -> b) -> (b -> a -> c) -> a -> c
applyBoth f g a = g (f a) a

halfSize :: [a] -> Int
halfSize = flip (div . length) 2

splitInHalf :: [a] -> ([a], [a])
splitInHalf = applyBoth halfSize splitAt

countInversions :: Ord a => [a] -> (Int, [a])
countInversions [] = (0, [])
countInversions [x] = (0, [x])
countInversions xs =
  let (left, right) = splitInHalf xs
      (leftN, mergedLeft) = countInversions left
      (rightN, mergedRight) = countInversions right
      (finalN, merged) = mergeCount mergedLeft mergedRight
   in (leftN + rightN + finalN, merged)
