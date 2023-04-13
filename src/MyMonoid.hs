module MyMonoid (
  MyPair(..)
) where

newtype MyPair b a = MyPair { getPair :: (a, b) } deriving (Show)

instance Functor (MyPair c) where
  fmap f (MyPair (x, y)) = MyPair (f x, y)
