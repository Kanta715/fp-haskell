module Baby(baby, printLine, double, doubleUs, doubleSmallNumber) where

baby :: String -> String
baby x = x ++ "__Baby__" ++ x

printLine :: String -> IO()
printLine x = putStrLn x

double :: Int -> Int
double x = x * x

doubleUs :: Int -> Int -> Int
doubleUs x y = double x + double y

doubleSmallNumber :: Int -> Int
doubleSmallNumber x = if  (x > 100)
                      then x
                      else double x
