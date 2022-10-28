module Chapter16 where

class MFunctor f where
  mfmap :: (a -> b) -> f a -> f b

data FixMePls a
  = FixMe
  | Pls a
  deriving (Eq, Show)

instance Functor FixMePls where
  fmap _ FixMe = FixMe
  fmap f (Pls a) = Pls (f a)