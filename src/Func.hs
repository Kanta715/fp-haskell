module Func(
  Composite(..),
  plus10Com,
  plus10,
  toString
) where

data Composite from to = Composite (from -> to)

instance Functor (Composite from) where
  fmap f (Composite g) = Composite (\x -> f (g x))

plus10Com :: Composite Int Int
plus10Com = Composite plus10

plus10 :: Int -> Int
plus10 n = n + 10

toString :: (Show a) => a -> String
toString a = show a
