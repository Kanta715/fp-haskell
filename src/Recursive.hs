module Recursive (
  maximum',
  replicate',
  take',
  reverse',
  repeat',
  zip',
  elem',
  quicksort
) where

maximum' :: (Ord o) => [o] -> o
maximum' []     = error "Error: Empty list!"
maximum' [o]    = o
maximum' (o:ol) = max o (maximum' ol)

replicate' :: Int -> a -> [a]
replicate' int val
 | int <= 0 = []
 | otherwise = val : replicate' (int - 1) val

take' :: Int -> [a] -> [a]
take' int _
  | int <= 0          = []
take' _ []            = []
take' int (head:tail) = head : take' (int - 1) tail

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (head:tail) = reverse' tail ++ [head]

repeat' :: a -> [a]
repeat' a = a : repeat' a

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (h2:t2) = [(h, h2)] ++ zip' t t2

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' v (head:tail)
  | v == head = True
  | otherwise = elem' v tail

quicksort :: (Ord a) => [a] -> [a]
quicksort []          = []
quicksort [a]         = [a]
quicksort (head:tail) =
  let
    smallOrEqual = [s | s <- tail, s <= head]
    large        = [l | l <-   tail, l  > head]
  in
    quicksort smallOrEqual ++ [head] ++ quicksort large
