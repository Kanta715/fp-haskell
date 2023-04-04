module Main (main) where

import Data.Char

main :: IO ()
main = do
  contents <- getContents
  putStrLn contents
  putStrLn . map toUpper $ contents
