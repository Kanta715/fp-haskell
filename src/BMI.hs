module BMI(bmi) where

bmi :: Double -> String
bmi num
  | num <= 18.5 = "gari"
  | num <= 25.0 = "futsu"
  | num <= 30.0 = "pocchari"
  | otherwise   = "debu"

bmi :: Double -> Double -> String
bmi weight height
  | num <= 18.5 = "gari"
  | num <= 25.0 = "futsu"
  | num <= 30.0 = "pocchari"
  | otherwise   = "debu"
  where num = weight / height ^ 2