## 関数の構文

### パターンマッチ
上から下の順で試される。<br>
渡された値がしてされたパターンと合致すると、対応する関数の本体が使われる。<br>
具体的な値（7）などではなく、x, y など小文字から始まる名前を書くと、任意の値に合致するようになる。
```haskell
-- 7 が渡されたら "A" を返す
-- それ以外は "B" を返す
-- x を 7 よりも前に持ってきてしまうと、x に合致してしまう
ghci> l :: Int -> String; l 7 = "A"; l x = "B"
ghci> l 2
"B"
ghci> l 7
"A"

-- 予期せぬ値が渡された場合はエラーを返す
ghci> c :: Char -> String; c 'a' = "A"; c 'b' = "B"
ghci> c 'a'
"A"
ghci> c 'c'
"*** Exception: <interactive>:6:22-45: Non-exhaustive patterns in function c

-- タプルもいける
-- _ は使い捨て変数として使える
ghci> first :: (a,b,c) -> a; first (x, _, _) = x
ghci> first ("String", 'C', 1000)
"String"

-- リスト内包表記のパターンマッチでは、失敗したら次の要素に進み、失敗した要素は結果のリストには含まれない
ghci> let list = [(1,2), (1,3), (1,4), (2,3), (2,4)]
ghci> [a+100 | (a, 3) <- list]
[101,102]

-- リストのパターンマッチ
ghci> head' :: [h] -> h; head' [] = error "Error!!!"; head' (x:_) = x
ghci> head' ['a', 'b']
'a'
ghci> head' []
*** Exception: Error!!!
CallStack (from HasCallStack):
  error, called at <interactive>:32:31 in interactive:Ghci10
  
-- as パターン
-- @ の前に定義した変数を関数の本体で使用できる
ghci> firstLetter :: String -> String; firstLetter "" = "Error";  firstLetter all@(x: xs) = "The first letter of " ++ all ++  " is " ++ [x]
ghci> firstLetter ""
"Error"
ghci> firstLetter "AIUEO"
"The first letter of AIUEO is A"
```

### ガード、where、let in, case
パイプで区切る<br>
otherwise で全てをキャッチする<br>
= がいきなりなくなる<br>
where で変数に束縛できる
```haskell
bmi :: Double -> String
bmi num
  | num <= 18.5 = "gari"
  | num <= 25.0 = "futsu"
  | num <= 30.0 = "pocchari"
  | otherwise   = "debu"

ghci> bmi 1.1
"gari"
ghci> bmi 36.6
"debu"

-- where で変数を使って関数を定義
bmi :: Double -> Double -> String
bmi weight height
  | num <= 18.5 = "gari"
  | num <= 25.0 = "futsu"
  | num <= 30.0 = "pocchari"
  | otherwise   = "debu"
  where num = weight / height ^ 2
ghci> bmi 72 1.72
"futsu"

-- let
-- 構文: let [bindings] in [expression]
-- let は where とは似ているが先に束縛をして、後に式を書く
-- let はローカルスコープに関数を作ったりもできる
-- let は式であり局所的なため、、ガードを跨いで使うことができない
cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in sideArea + 2 * topArea

calcBmisOfLet :: [(Double, Double)] -> [Double]
calcBmisOfLet xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]
ghci> calcBmisOfLet [(72.0, 1.72)]
[24.337479718766904]

-- case
-- 構文: case [expression] of pattern -> result
--                           pattern -> result
--                             ⋮
-- 変数の指定した対する処理を定義できる & パターンマッチが使える
-- パターンが見つからない場合はエラーを吐く
```
