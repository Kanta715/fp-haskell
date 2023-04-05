module Main (main) where

import Data.Char
import System.IO

main :: IO ()
main = fileIOFunc2

ioFunc :: IO ()
ioFunc = do
  contents <- getContents
  putStrLn contents
  putStrLn . map toUpper $ contents

fileIOFunc :: IO ()
fileIOFunc = do
  handle   <- openFile "io/fileio2.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

fileIOFunc2 :: IO ()
fileIOFunc2 = do
  withFile "io/fileio2.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStr contents
