module Curry(
  compareWith10,
  divideByTen,
  applyTwice,
  zipWith',
  flip',
  listOfFunc,
  reverse'
) where

compareWith10 :: (Ord a, Num a) => [a] -> [a]
compareWith10 []   = []
compareWith10 list = [max 10 a | a <- list]

-- セクション
divideByTen :: (Floating a) => a -> a
divideByTen =  (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

listOfFunc :: [String] -> [(String -> String)]
listOfFunc []   = []
listOfFunc list = [combinationNumber str | str <- list]

combinationNumber :: String -> String -> String
combinationNumber str str2
  | length str <= 5  = "1. " ++ str ++ str2
  | length str <= 10 = "2. " ++ str ++ str2
  | length str <= 15 = "3. " ++ str ++ str2
  | otherwise        = "0. " ++ str ++ str2

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
