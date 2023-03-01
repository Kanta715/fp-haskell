## 型

### 型クラス
型クラスは、何らかの振る舞いを定義するインターフェイスである。ある型クラスのインスタンスである型は、その型クラスが記述する振る舞いを実装する。<br>
型クラスは関数の集まりを定める。それらの関数がその型でどういった意味を成すのかを定義する。
```haskell
ghci> :t (==)
-- => よりも前位に貼るものは型クラス制約と呼ばれる。
-- Eq 型クラスのインスタンスである任意の a型を2つ引数として受け取り Bool で返す
-- NOTE: オブジェクト指向のクラスとは違うらしい
(==) :: Eq a => a -> a -> Bool
```

#### Eq型
型が関数を実装しているとは、その関数が特定の型に対して使われた時に、どういう振る舞いをするかを定義するということ。
```haskell
5 == 5
True
5 /= 5
False
'a' == 'a'
True
3.33 == 3.33
True
```

#### Ord型
Ord は何らかの順序をつけられる型のための型クラス<br>
`>, <, >=, <=` などをサポートしている<br>
Ord a を 2つ受け取り Ordering とかいうやつを返す compare もあるらしい
```haskell
:t (>)
(>) :: Ord a => a -> a -> Bool

:t compare
compare :: Ord a => a -> a -> Ordering

// ??
ghci> compare 1 2
LT
```


#### Show型
ある値は、その型が Show型クラスのインスタンスの場合、文字列として表現できる。
```haskell
ghci> show 4
"4"
```

#### Read型
Read は Show と対をなす型クラス<br>
read 関数は文字列を受け取り、Read インスタンスの方の値を返す
```haskell
-- 文字列渡すだけだと推論できない
ghci> read "1"
*** Exception: Prelude.read: no parse

-- 推論できるような式を渡す
ghci> read "1" + 2
3
-- 型注釈を渡す
ghci> read "1" :: Int
1
```

#### Enum型
要素を列挙できる型<br>
Enum型クラスは、その値をレンジの中で使える。また、`succ`,`pred` も定義される。<br>
`(), Char, Bool, Ordering, Int, Integer, Float, Double`など
```haskell
ghci> ['a'..'e']
"abcde"
ghci> succ 'B'
'C'
ghci> pred 'B'
'A'
```

#### Bounded型
上限と下限を持ち、それぞれ `minBound, maxBound` 関数で調べられる。
```haskell
ghci> minBound :: Int
-9223372036854775808
ghci> maxBound:: Int
9223372036854775807
```

#### Num型
数の型クラス<br>
Num型クラスの任意のインスタンス（Int, Integer, Float, Double など）として振る舞える<br>
ある型を Num型のインスタンスにするには、その型が既に Show と Eq のインスタンスになっている必要がある。
```haskell
:t 20
20 :: Num a => a
ghci> 20 :: Int
20
ghci> 20 :: Integer
20
ghci> 20 :: Float
20.0
ghci> 20 :: Double
20.0
ghci> :t (*)
(*) :: Num a => a -> a -> a
```

#### Integral型
Integral も数の型クラス<br>
Num が実装を含む全ての数を含む一方、Integral は整数のみが含まれる(Int, Integer のみ含む)
```haskell
 :t fromIntegral
fromIntegral :: (Integral a, Num b) => a -> b

-- length は Int を返すため、Integral と Num を * で計算することはできない（* は同じ型をとるため）
ghci> length ['a', 'a', 'a'] * 3.2
<interactive>:66:26: error:
    • No instance for (Fractional Int) arising from the literal ‘3.2’
    • In the second argument of ‘(*)’, namely ‘3.2’
      In the expression: length ['a', 'a', 'a'] * 3.2
      In an equation for ‘it’: it = length ['a', 'a', 'a'] * 3.2
ghci> :t length
length :: Foldable t => t a -> Int

-- fromIntegral で Integral から Num に変換して計算することでエラーを吐かずに済む
fromIntegral (length ['a', 'a', 'a']) * 3.2
9.600000000000001
```
