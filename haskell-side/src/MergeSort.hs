module MergeSort where

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge as [] = as
merge [] bs = bs
merge (a : as) (b : bs)
  | a < b = a : merge as (b : bs)
  | otherwise = b : merge (a : as) bs

applyBoth :: (a -> b) -> (b -> a -> c) -> a -> c
applyBoth f g a = g (f a) a

halfSize :: [a] -> Int
halfSize = flip (div . length) 2

splitInHalf :: [a] -> ([a], [a])
splitInHalf = applyBoth halfSize splitAt

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs =
  let (left, right) = splitInHalf xs
   in merge (mergeSort left) (mergeSort right)
