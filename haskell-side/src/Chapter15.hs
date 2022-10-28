module Chapter15 where

import Data.Monoid
import Data.Semigroup

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada Nada = Nada
  (<>) Nada (Only a) = Only a
  (<>) (Only a) Nada = Only a
  (<>) (Only a) (Only b) = Only (a <> b)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend Nada Nada = Nada
  mappend Nada (Only a) = Only a
  mappend (Only a) Nada = Only a
  mappend (Only a) (Only b) = Only (mappend a b)

optionalA = Only (Sum 1)

optionalB = Only (Sum 2)

appended = mappend optionalA optionalB

newtype Combine a b = Combine {unCombine :: a -> b}

instance Semigroup b => Semigroup (Combine a b) where
  (<>) (Combine f) (Combine g) = Combine (f <> g)

f = Combine $ \n -> Sum (n + 1)

g = Combine $ \n -> Sum (n - 1)

unCombined = unCombine (f <> g) 0
