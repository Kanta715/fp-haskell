module BMI(bmi, calcBmis, calcBmisOfLet, bmiForCase) where

bmi :: Double -> Double -> String
bmi weight height
  | num <= 18.5 = "gari"
  | num <= 25.0 = "futsu"
  | num <= 30.0 = "pocchari"
  | otherwise   = "debu"
  where num = weight / height ^ 2

bmiForCase :: Double -> Double -> String
bmiForCase weight height =
  let str = bmi weight height
  in case str of "gari"     -> "Make sure you eat well."
                 "futsu"    -> "No dietary problems."
                 "pocchari" -> "Let's cut back on calories a bit."
                 "debu"     -> "Let's go straight to the hospital."
                 x          -> "BMI could not be measured correctly."

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [calc w h | (w ,h) <- xs]
  where calc weight height = weight / height ^ 2

calcBmisOfLet :: [(Double, Double)] -> [Double]
calcBmisOfLet xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
