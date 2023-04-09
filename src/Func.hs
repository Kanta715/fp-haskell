module Func(
  Func(..),
  plus10Com,
  plus10,
  toString
) where

data Func from to = Func (from -> to)

instance Functor (Func from) where
  -- . 関数と同じことをする
  fmap f (Func g) = Func (\x -> f (g x))

plus10Com :: Func Int Int
plus10Com = Func plus10

plus10 :: Int -> Int
plus10 n = n + 10

toString :: (Show a) => a -> String
toString a = show a
