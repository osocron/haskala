module Chapter10 where

mfoldr :: (a -> b -> b) -> b -> [a] -> b
mfoldr f b [] = b
mfoldr f b (head : tail) = mfoldr f (f head b) tail

mfoldr2 :: (a -> b -> b) -> b -> [a] -> b
mfoldr2 f b [] = b
mfoldr2 f b (head : tail) = f head (mfoldr f b tail)

msum :: [Integer] -> Integer
msum = mfoldr (+) 0

-- mlength :: [a] -> Integer
-- mlength = undefined

-- mproduct :: [Integer] -> Integer
-- mproduct [] = 1
-- mproduct (x : xs) = x * mproduct xs

-- nconcat :: [[a]] -> [a]
-- nconcat [] = []
-- nconcat (x : xs) = x ++ nconcat xs

sumInts :: [Integer] -> Integer
sumInts = mfoldr (+) 0

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data MEither e a = MLeft e | MRight a deriving (Eq, Show)

instance Functor (MEither e) where
  fmap f (MRight x) = MRight (f x)
  fmap _ (MLeft x) = MLeft x

data ITree a
  = Leaf (Int -> a)
  | Node [ITree a]

instance Functor ITree where
  fmap f (Leaf x) = Leaf (f . x)
  fmap f (Node xs) = Node (fmap (fmap f) xs)

instance Show a => Show (ITree a) where
  show (Leaf x) = "Reached a Leaf"
  show (Node xs) = show $ fmap show xs
