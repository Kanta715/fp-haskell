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

## 型や型クラスを作る
`data` キーワードで独自型を作成可能。等価符号の前が**型名**、後ろが**値コンストラクタ**。<br>
値コンストラクタは、型ではなくただインスタンスを生成するための関数。<br>
Haskell では Show 型のインスタンスじゃないと出力時にエラー出るので、`deriving` を書いてやれば関数など適用した後にエラーを吐かずに値を出力してくれる。<br>
`deriving` と宣言してやると `deriving` の後に書いた型でインスタンスを生成してくれる。（`()` で囲む）<br>
レコード構文で宣言すると値に簡単にアクセス可能になる。値を取得する関数を自動で定義してくれる。（独自型は基本的にはこっちの方が使いやすそう）ちょっと残念なのは部分適用できないこと。全てのフィールドを同時に指定しなければならない。
```haskell
-- 値コンストクタのエクスポート: Name(LastAndFirst, LastAndMiddleAndFirst) or Name(..): 後者は全てが対象になる
ghci> data Name = LastAndFirst String String | LastAndMiddleAndFirst String String String

-- String を取って String を取って Name 型のインスタンスを返す or String を取って String を取って String を取って Name 型のインスタンスを返す 関数
ghci> :t LastAndFirst
LastAndFirst :: String -> String -> Name
ghci> :t LastAndMiddleAndFirst
LastAndMiddleAndFirst :: String -> String -> String -> Name

ghci> let n = LastAndFirst "Ohtani" "Shohei"
ghci> toTanaka n
"TanakaShohei"
-- 出力できない
ghci> n
-- <interactive>:8:1: error:
--  • No instance for (Show Name) arising from a use of ‘print’
--  • In a stmt of an interactive GHCi command: print it
--

-- deriving (Show) としてやるとエラーが出ない
ghci> let n = LastAndFirst "Ohtani" "Shohei"
ghci> n
LastAndFirst "Ohtani" "Shohei"

ghci> toTanaka $ LastAndMiddleAndFirst "Taylor" "Tatsuji" "Nootbaar"
"TanakaTatsujiNootbaar"

-- 部分適用もいける
-- タツジのクローンをいっぱい作る
ghci> let cloneTatsujis = map (LastAndMiddleAndFirst "Taylor" "Tatsuji") ["AAA", "BBB", "CCC", "DDD", "EEE"]
-- タツジを全員田中にする
ghci> map (toTanaka) cloneTatsujis
["TanakaTatsujiAAA","TanakaTatsujiBBB","TanakaTatsujiCCC","TanakaTatsujiDDD","TanakaTatsujiEEE"]

-- 独自型でユーザー型を定義
ghci> let name  = LastAndMiddleAndFirst "Taylor" "Tatsuji" "Nootbaar"
ghci> let year  = BirthYear 1990
ghci> let month = BirthMonth 12
ghci> let day   = BirthDay 25
ghci> let user :: User = User name year month day
ghci> user
User (LastAndMiddleAndFirst "Taylor" "Tatsuji" "Nootbaar") (BirthYear 1990) (BirthMonth 12) (BirthDay 25)

-- 部分適用: Haskell は全てカリー化されている
ghci> let nameOnly = User name
ghci> :t nameOnly
nameOnly :: BirthYear -> BirthMonth -> BirthDay -> User

data Name = LastAndFirst          String String |
            LastAndMiddleAndFirst String String String
            deriving (Show)
data Age = Age Int deriving (Show)
-- レコード構文で書くと、name, age などの関数が自動で定義される
data Person = Person {
  name :: Name,
  age  :: Age
} deriving (Show)

ghci> let person :: Person = Person { name = LastAndFirst  "Ohtani" "Shohei", age = Age 28 }
ghci> person
Person {name = LastAndFirst "Ohtani" "Shohei", age = Age 28}
ghci> name person
LastAndFirst "Ohtani" "Shohei"
ghci> age person
Age 28
```

## 型引数
以下は公式の `Maybe` の定義<br>
`Maybe` は型引数``a``を取り、`Nothing` or `Just a` として返す。`Maybe` 自体は、**型** ではなく、**型コンストラクタ** である。何らかしらの型を引数として受け取り、それに応じて `Maybe x` という型を作る。 
そのため、単なる `Maybe` 型は存在しない。<br>
**型コンストラクタ**は全ての引数を埋めてから初めて何らかしらの値の型になれる。<br>
Q. 型コンストラクタは、汎用的に様々な型からなる値のインスタンスを作成できるが、メリットは何なのか？<br>
A. データ型自体の動作にそこまで影響を与えずに、データ型を箱と見たい時。（たぶん1ヶ月後に見たら意味わからんことになってるので、自分なりの理解を補足として以下に記述）
 - 業務で使うようなデータ型は、基本的に汎用的というよりも取りたい引数の型が決まっていることが多い
   - 例: User 型
     - Id
     - Name
     - Age
     - こういった具体的な型ややりたい処理が決まっているものは、汎用的な箱として扱うよりも、独自の働きをする型として使用する
 - それに対して、`Maybe`, `List`, `Map` などは何らかの型の値が与えられ、その時作成されたデータ型を便利な箱として汎用的な処理をしたい時に使用する。
<br>
 
ちなみに、Haskell ではデータ宣言には型クラス制限をつけないというコーディング規約がある。<br>
やりたいことがある場合は、関数実装時に型制約するからデータ宣言で制限入れてもメリットが少ないかららしい。
```haskell
data  Maybe a  =  Nothing | Just a

-- 与えられる型によって、型が変化する
ghci> :t Nothing
Nothing :: Maybe a
ghci> :t Just "AIUEO"
Just "AIUEO" :: Maybe String
ghci> :t Just ['a','a']
Just ['a','a'] :: Maybe [Char]
ghci> :t Just 32

-- 様々な型を格納するデータ型を作れる
-- 中身ごとに別々の型にしたりもできる
ghci> data IntMaybe = IntNothing | IntJust Int
ghci> :t IntNothing
IntNothing :: IntMaybe
ghci> :t IntJust
IntJust :: Int -> IntMaybe
```
