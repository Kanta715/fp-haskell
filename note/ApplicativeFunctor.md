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

アプリカティブファンクターは `pure` と　`<*>` を定義している。<br>
どちらもデフォルト実装はないため、インスタンスにする場合は両方の定義を洗える必要がある。<br>
**Applicative のクラス定義**
- f: Functor のインスタンスでなければならないという型制約がある（この f は確実に fmap が使える）
- pure: 任意の型を受け取り、Applicative(Functor) 値の中に入れて返す
- <*>
  - fmap の強化版<br>
  - 関数の入っている Applicative(Functor) を取り、関数を Applicative(Functor) 値に適用して、Applicative(Functor) 値を返す
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
<$> は (a -> b) の関数と Applicative(Functor) の値を取って、Applicative(Functor) として返してくれる。
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
