module Curry(
  compareWith10,
  divideByTen
) where

compareWith10 :: (Ord a, Num a) => [a] -> [a]
compareWith10 []   = []
compareWith10 list = [max 10 a | a <- list]

-- セクション
divideByTen :: (Floating a) => a -> a
divideByTen =  (/10)
