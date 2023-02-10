module Main (main) where

import Baby

main :: IO ()
main =
  do
    x <- getLine
    printLine (baby x)
