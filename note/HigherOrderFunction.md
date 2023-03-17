## 高階関数
引数として関数を受け取ったり、返り値として関数を返す関数を**高階関数**と呼ぶ。

### カリー化関数
カリー化された関数は複数の引数を取る代わりに1つの引数を取り、その次の引数を取る関数を返す。Haskell ではカリー化関数によって、複数の引数を取る関数を実現している。<br>
**部分適用**した関数はある関数に一定の制約を設けた関数を、手軽に作り出して使用できる。
```haskell
-- max 関数での例
-- max は2つの引数を取り、大きい値を返す
-- 2つの引数を同時に渡すように実装されているが実際は、
max 4 5
-- このように 4 を適用した後に、5 に適用するための新しい関数を返す
(max 4) 5

-- max 関数の型
-- a -> (a -> a) を見れば、a 型の値を取り「a 型の値を取り a 型の値を返す関数」を返すことがわかる
-- メリット: 部分適用された関数が得られる
ghci> :t max
max :: Ord a => a -> a -> a

-- 部分適用された関数を使うイメージ
-- 10以下のものは全て10にしたいが、それ以外のものはそのままにしたい
ghci> let compareWith10 = max 10
ghci> :t compareWith10
compareWith10 :: (Ord a, Num a) => a -> a

ghci> let ints = [12, 8, 10, 4, 20]
ghci> let moreThan10 = [compareWith10 int | int <- ints]
ghci> moreThan10
[12,10,10,10,20]

-- セクション
-- 中置換数は、()で囲むことで片方に引数を適用できる
-- マイナスは無理らしい
divideByTen :: (Floating a) => a -> a
divideByTen =  (/10)

-- 関数と値を取る関数
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

plusTen :: Int -> Int
plusTen x = x + 10

ghci> applyTwice plusTen 10
30

-- 部分適用
ghci> let plusTwenty = applyTwice plusTen
ghci> plusTwenty 10
30

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

numberTheItem :: Int -> String -> String
numberTheItem int str = (show int) ++ ". " ++ str

ghci> let ints :: [Int] = [1,2,3,4,5]
ghci> let strs = ["Red", "Blue", "Green", "Yellow", "White", "Black", "Orange"]
ghci> zipWith' numberTheItem ints strs
["1. Red","2. Blue","3. Green","4. Yellow","5. White"]

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x
ghci> let fliped = flip' numberTheItem
ghci> :t fliped
fliped :: String -> Int -> String

-- 関数リストを作る
listOfFunc :: [String] -> [(String -> String)]
listOfFunc []   = []
listOfFunc list = [combinationNumber str | str <- list]

combinationNumber :: String -> String -> String
combinationNumber str str2
  | length str <= 5  = "1. " ++ str ++ str2
  | length str <= 10 = "2. " ++ str ++ str2
  | length str <= 15 = "3. " ++ str ++ str2
  | otherwise        = "0. " ++ str ++ str2

ghci> let funcList = listOfFunc ["AAAAAAA", "B", "CCCC", "DDDDDDDDDDDDDDDDDD", "EE", "FFF"]
ghci> :t funcList
funcList :: [String -> String]
ghci> [func "  OK" | func <- funcList]
["2. AAAAAAA  OK","1. B  OK","1. CCCC  OK","0. DDDDDDDDDDDDDDDDDD  OK","1. EE  OK","1. FFF  OK"]
```

### ラムダ式
**ラムダ式**とは、1回だけ必要な関数を作る時に使う無名関数のことを言う。<br>
通常、ラムダ式は高階関数に渡す関数を作るためだけに使われる。<br>
```haskell
-- 構文
-- \ を書いて、関数の引数をスペース区切りで書く
-- 通常、ラムダ式は括弧で囲む
-- いちいち関数を定義しなくていいから便利
ghci> zipWith' (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
[153.0,61.5,31.0,15.75,6.6]
```
