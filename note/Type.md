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

### 型引数
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
     - `data User = User Id Name Age deriving (Show)`
     - こういった具体的な型ややりたい処理が決まっているものは、汎用的な箱として扱うよりも、独自の働きをする型として使用する
 - それに対して、`Maybe`, `List`, `Map` などは何らかの型の値が与えられ、その時作成されたデータ型を便利な箱として汎用的な処理をしたい時に使用する。
<br>
 
ちなみに、Haskell ではデータ宣言には型クラス制限をつけないというコーディング規約がある。<br>
やりたいことがある場合は、関数実装時に型制約するからデータ宣言で制限入れてもメリットが少ないかららしい。<br>

値コンストラクタ：値を引数として取り、独自のデータ型のインスタンスを生成する関数<br>
型コンストラクタ：多相型を実現するもの<br>
`data 型コンストラクタ = 値コンストラクタ`
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

-- 型引数を2つ取る E 型は、L, R という値コンストラクタを持つ
-- l 型の引数を取った場合は、L 型のインスタンスを返す
-- r 型の引数を取った場合は、R 型のインスタンスを返す
-- 型コンストラクタはただのシノニム（別名）で、値コンストラクタは引数を取りデータ型を返す関数ということがよくわかる
data E l r = L l | R r deriving (Show)
ghci> let e :: E Int String = R "Right"
ghci> e
R "Right"
ghci> :t e
e :: E Int String
```

### インスタンスの自動導出
オブジェクト指向のクラスは、表現したいものを抽象化したものと考えることができる。（インスタンスはそれを具象化したもの）<br>
Haskell のクラスの考え方は、まずデータ型を作り、それから「このデータには何ができるか」を考える。もしその型が、等価性をテストできるものだった場合は、`Eq` 型クラスのインスタンスにする。<br>
Haskell では特定の型クラスのインスタンス宣言を自動導出できる。（Eq, Ord, Enum, Bounded, Show, Read）ちなみに、値コンストラクタの中の全てのフィールドが自動導出（deriving）に指定しているインスタンスでなければならない。<br>
`Eq` を自動導出する場合は、順番に気をつける（値コンストラクタの前に位置する方が小さい値とみなされる）
```haskell
-- False の方が小さいとみなされる
data Bool = False | True
```

### 型シノニム
`[Char]` と `String` は同値で交換可能である。これは**型シノニム（型同義名）**を使って実装されている（らしい）。<br>
型シノニムそのものは特に何もせず、ただある型に対して別の名前を与えている。<br>
新しいデータ型宣言　　：`data`<br>
既存の型をシノニム宣言：`type`<br>

ある型に別名をつけて直感的に何を表しているのか分かりやすくする時に便利
```haskell
type String = [Char]

-- 同じ
ghci> a :: [Char] -> [Char]; a x = x ++ "----"
ghci> b :: String -> String; b x = x ++ "==="

ghci> let c :: [Char] = "CCC"
ghci> let s :: String = "SSS"

ghci> a c
"CCC----"
ghci> a s
"SSS----"

ghci> b c
"CCC==="
ghci> b s
"SSS==="

type A = String
type B = Int
type C = (String, Int)
cFunc :: C -> A
cFunc (a, b) = a

ghci> let a :: A = "KKK"
ghci> let b :: B = 100
ghci> let c :: C = (a, b)
ghci> cFunc c
"KKK"

-- 多相もいける
type IntTuple v = (Int, v)
ghci> let tuple :: IntTuple String = (1, "Aieeo")
tuple :: IntTuple String
```

### 再帰的なデータ構造
```haskell
data MyList a = Nil | List a (MyList a) deriving (Show)

ghci> List 1 (List 3 (List 10 Nil))
ghci> Nil

-- 二分探索木
data Tree a = None | Node a (Tree a) (Tree a) deriving (Show)

ghci> let node :: Tree Int = Node 5 (Node 3 (Node 1 (Node 0 (None) (None)) (None)) (Node 4 (None) (None))) (None)
ghci> node
Node 5 (Node 3 (Node 1 (Node 0 None None) None) (Node 4 None None)) None

singleton :: a -> Tree a
singleton a = Node a None None

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert a None = singleton a
treeInsert a (Node x left right)
 | a == x = Node x left right
 | a  < x = Node x (treeInsert a left) right
 | a  > x = Node x left (treeInsert a right)

haveInTree :: (Ord a) => a -> Tree a -> Bool
haveInTree a None = False
haveInTree a (Node v left right)
 | a == v = True
 | a  < v = haveInTree a left
 | a  > v = haveInTree a right

ghci> let nums = [1,3,4,6,78,8,5,4,3,3,4,5,6,6,6,6,10]
ghci> let numTree = foldr treeInsert None nums
ghci> numTree
Node 10 (Node 6 (Node 5 (Node 4 (Node 3 (Node 1 None None) None) None) None) (Node 8 None None)) (Node 78 None None)

ghci> haveInTree 100 numTree
False

ghci> haveInTree 10 numTree
True
```

### 型クラスを使ってインスタンスを作る
構文：`instance {型クラス} {型} where`
`Eq` 型クラスのインスタンスを作りたい場合、**最小完全定義**を定義する。<br>
`Eq` の場合は `==` or `/=` のどちらかを実装する。
```haskell
data Supports = Soccer | Tennis | Baseball

instance Eq Supports where
  (==) Soccer Soccer     = True
  (==) Tennis Tennis     = True
  (==) Baseball Baseball = True
  (==) _        _        = False

ghci> Baseball == Baseball
True
ghci> Baseball == Soccer
False

-- 独自の真偽判定をしてくれる型クラスとかも作れる
class YesNo a where
  yesno :: a -> Bool

instance YesNo Supports where
  yesno Soccer   = False
  yesno Tennis   = False
  yesno Baseball = True

ghci> yesno Baseball
True
ghci> yesno Soccer
False
ghci> yesno Tennis
False
ghci> yesno a
```

#### Functor 型クラス
Functor 型クラスは fmap を最小完全定義として持っている。（デフォルト実装がないため、Functor 型クラスのインスタンスを作りたい時は fmap を実装しなければならない）<br>
`f` は具体型ではなく1つの引数をとる型コンストラクタになっている。<br>
**a 型から b 型への関数** と **a 型に適用された Functor** を取り、**b 型に適用された Functor** を返す
```haskell
-- f a, f b を見てわかるように、f は1つの型引数を取る型コンストラクタ
ghci> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b

-- E を Functor のインスタンスにしたい場合は、1つ型引数を適用したもの（L or R）として渡してやらなければならない 
-- E は 2 つの型引数をとる型コンストラクタだということがわかる（Functor f の f は、1つの型引数を取る型コンストラクタ）
instance Functor (E a) where
  fmap f (L a) = L a
  fmap f (R a) = R (f a)

ghci> :k E
E :: * -> * -> *
-- Left, Right は 1つの型が適用された E である
ghci> :t L
L :: l -> E l r
ghci> :t R
R :: r -> E l r

ghci> let l = Left "Left"
ghci> let r = Right "Right"
ghci> :t l
l :: Either String b
ghci> :t r
r :: Either a String

ghci>  baby :: String -> String; baby x = x ++ "__Baby__" ++ x
ghci> fmap baby l
Left "Left"
ghci> fmap baby r
Right "Right__Baby__Right"
```

### 型を司るもの、種類
「種類」とは**型の型**を表す。* （スター）は具体型（型引数を取らない型）を表す記号。<br>
`:t`: 値の型を調べる時に使う<br>
`:k`: 型の種類を調べる時に使う
```haskell
-- 具体型
ghci> :k Int
Int :: *

-- 具体型を取り、具体型を返す型コンストラクタ（Int -> Maybe Int）
ghci> :k Maybe
Maybe :: * -> *

-- Maybe Int は具体型なことがわかる
ghci> :k Maybe Int
Maybe Int :: *
```
