## アプリカティブファンクター
`Functor` は 1引数関数でファンクターを写すために使うことが多い。仮に2引数関数でファンクターを写すと `Functor a -> b` となる。<br>
**その中身の関数を引数に取れる関数**を定義するくらししか使えない
```haskell
-- Maybe (a -> a) になる
ghci> let f = fmap (*) (Just 20)
ghci> :t f
f :: Num a => Maybe (a -> a)

-- その中身の関数を引数に取れる関数を定義すれば使えるっちゃ使える
ghci> fmap (\f -> f 100) f
Just 2000
```

アプリカティブファンクターは `pure` と `<*>` を定義している。<br>
どちらもデフォルト実装はないため、インスタンスにする場合は両方の定義をする必要がある。<br>
**Applicative のクラス定義**
- f: Functor のインスタンスでなければならないという型制約がある（この f は確実に fmap が使える）
- pure: 任意の型を受け取り、Applicative 値の中に入れて返す
- <*>
  - fmap の強化版<br>
  - 関数の入っている Applicative を取り、関数を Applicative 値に適用して、Applicative 値を返す
```haskell
-- クラス定義
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

**Maybe の実装**
- pure: ただ値を取って `Just` でラップして返す
- <*>
  - Just の時は、最初にとる `Just (a -> b)` から、関数のみを取り出して `fmap` にそのまま適用する
  - Nothing の場合は、Nothing を返す
```haskell
instance Applicative Maybe where
    pure = Just

    Just f  <*> m       = fmap f m
    Nothing <*> _m      = Nothing
```

使ってみる<br>
```haskell
-- 何の pure 使っとるかわからんから型引数で教えてあげる
ghci> let p :: Maybe Int = pure 100

ghci> p
Just 100

ghci> let jPlus10 = Just plus10
ghci> :t jPlus10
jPlus10 :: Maybe (Int -> Int)

ghci> let j = jPlus10 <*> p
ghci> j
Just 110
```

## アプリカティブ・スタイル
Applicative 型クラスでは <*> 関数を連続して使うことができる。<br>
`pure f <*> x` = `fmap f x` になる。
```haskell
ghci> let f :: Maybe (Int -> Int -> Int) = pure (+)
ghci> f <*> Just 20 <*> Just 200
Just 220

-----------------------
-- ↑のやってること
ghci> let g = f <*> Just 20
ghci> :t g
g :: Maybe (Int -> Int)

ghci> let a = g <*> Just 200
ghci> a
Just 220
```

#### <$>
<$> を使うことでさらにアプリカティブスタイルが使いやすくなる。<br>
<$> は (a -> b) の関数と Applicative の値を取って、Applicative として返してくれる。<br>
普通の関数を Applicative に適用したい時は <$> <*> だけで済む。
```haskell
(<$>) :: Functor f => (a -> b) -> f a -> f b
f <$> x = fmap f x

ghci> let a = (+) <$> Just 2
ghci> :t a
a :: Num a => Maybe (a -> a)

ghci> let b = (+) <$> Just 2 <*> Just 10
ghci> b
Just 12
```

### リスト
リストも Applicative のインスタンスになっている。<br>
```haskell
instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]
```

pure: ただ値を取ってリストに入れて返す<br>
<*>: 関数とリストを取り、リストの値に関数を適用して返す<br>
使ってみる
```haskell
ghci> let p :: [] Int = pure 100
ghci> p
[100]

-- 関数をリストに入れて <*> で適用する
-- Applicative の関数を Applicative の値に適用する
ghci> let result = [(\x -> x + 100)] <*> p
ghci> result
[200]
ghci> :t p

-- <$> を使うとわざわざ関数をリストにしなくてもOK
-- 普通の関数を Applicative の値に適用する
ghci> let result2 = (\x -> x + 100) <$> p
ghci> result2
[200]

-- 空の場合は、ちゃんと空が返ってくる
ghci> let result3 = (\x -> x + 100) <$> []
ghci> result3
[]

-- 引数に取る全ての関数を全ての値に適用してくれる
ghci> let result4 = [(+), (-), (*), (/)] <*> [100, 200, 300, 400] <*> [4, 20, 7, 50]
ghci> result4
[104.0,120.0,107.0,150.0,204.0,220.0,207.0,250.0,304.0,320.0,307.0,350.0,404.0,420.0,407.0,450.0,96.0,80.0,93.0,50.0,196.0,180.0,193.0,150.0,296.0,280.0,293.0,250.0,396.0,380.0,393.0,350.0,400.0,2000.0,700.0,5000.0,800.0,4000.0,1400.0,10000.0,1200.0,6000.0,2100.0,15000.0,1600.0,8000.0,2800.0,20000.0,25.0,5.0,14.285714285714286,2.0,50.0,10.0,28.571428571428573,4.0,75.0,15.0,42.857142857142854,6.0,100.0,20.0,57.142857142857146,8.0]

ghci> let result5 = (++) <$> ["I am XXX"] <*> ["!", "?", "...", "^-^"]
ghci> result5
["I am XXX!","I am XXX?","I am XXX...","I am XXX^-^"]
```

### IO も Applicative
IO は Functor のインスタンスであり Applicative のインスタンスにもなっている。<br>
pure: return(IO()を返すだけ)
<*>: IO(関数)をIO(値)に適用して返してくれる
```haskell
instance Applicative IO where
    pure = return
    a <*> b = do
        f <- a
        x <- b
        return (f x)
```

### 関数も Applicative
関数も Applicative のインスタンスになっている。<br>
pure: 値を取って、引数に何を渡してもその値を返す関数を返す（いつ使うのこれ）
<*>: 関数と関数を取り、引数に渡した値に関数を適用して返す
```haskell
instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)

ghci> let f :: ((->) String String) = pure "AIUEO"
ghci> :t f
f :: String -> String
ghci> f "DDD"
"AIUEO"

-- Applicative(関数) <*> Applicative(値) だから
ghci> let f = (+) <*> (*100)
ghci> :t f
f :: Num a => a -> a
-- f = (\x -> x * 100) + x になる
ghci> let result = f 100
ghci> result
10100

-- こっちは 関数 <$> Applicative(値) だから
ghci> let g = (+) <$> (+100)
ghci> :t g
g :: Num a => a -> a -> a
-- g = (\x -> x + 100) + x になる
ghci> let h = g <*> (+500)
ghci> :t h
h :: Num a => a -> a
-- h = (\x -> x + 100) + (\x -> x + 500) になる
ghci> let result2 = h 10
ghci> result2
620
```
