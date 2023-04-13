## モノイド
モノイドは、値を二項演算子で結合できるような型を表す。<br>

### 既存の型を新しい型に包む
`newtype` キーワードを使って既存の型から新たな型を作ることができる。<br>
**なぜ `newtype` を使うのか**
- 高速
  - data はコンストラクタに包んだりする度にオーバーヘッドがかかる（らしい）
  - newtype は「既存の型を包んで作った新しい型だから、内部処理は同じまま違う型を持たせれば良い」ということが Haskell に伝えることができる（らしい）
  - Haskell は型推論を済ませた後、コンストラクタに包んだり解いたりする処理を省略してくれる
**なぜ `newtype` を使わないのか**
- 制約
  - newtype
    - 値コンストラクタは1種類
    - 値コンストラクタのフィールドは1つ
  - data
    - 値コンストラクタは何種類でもOK
    - 値コンストラクタのフィールドは何個でもOK
```haskell
-- data
-- 既存の型を使い別の型で包む
-- 複数種類のの値コンストラクタ、複数の値コンストラクタのフィールドを持つことができる
ghci> data EE a b c d = LL a b | RR c d deriving (Eq, Show)
ghci> let ee :: EE Int String String Int = LL 1 "AAA"
ghci> ee
LL 1 "AAA"


-- newtype
-- 既存の型を使い別の型で包む
-- 値コンストラクタは1種類、値コンストラクタのフィールドは1つ
ghci> newtype NewType a = NewType { getValue :: a } deriving (Eq, Show)
ghci> let newEE = NewType ee
ghci> newEE
NewType {getValue = LL 1 "AAA"}

ghci> getValue newEE
LL 1 "AAA"

-- 自動生成される getValue 関数は Newtype a を受け取って a を返す単純な関数
ghci> :t getValue
getValue :: NewType a -> a

-- 値コンストラクタが複数ある or 値コンストラクタのフィールドが複数ある場合はエラー
ghci> newtype InvalidType a b = NewType { getA :: a, getB :: b  } deriving (Eq, Show)

<interactive>:19:27: error:
    • The constructor of a newtype must have exactly one field
        but ‘NewType’ has two
    • In the definition of data constructor ‘NewType’
      In the newtype declaration for ‘InvalidType’
```

### newtype を使って型クラスのインスタンスを作る
ある型を型クラスのインスタンスにしたいけど、型引数が一致しなくてできないことがある。<br>
Maybe は Functor 型クラスのインスタンスにするのは簡単だが、Tuple はどうするのか。<br>
Tuple の1つ目の値に関数を適用したい場合は、型コンストラクタを逆にしてやる。<br>
```haskell
-- Maybe は型引数を1つだけ取るため、簡単に Functor 型クラスのインスタンスにできる
-- f に Maybe が入る.
-- f は型引数を1つだけ取る
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing


-- Tuple の1つ目の値に関数を適用したい
newtype MyPair b a = MyPair { getPair :: (a, b) } deriving (Show)

-- MyPair c の c は2つ目の値の型
-- MyPair (x, y) は値コンストラクタだから MyPair String Int 型の値の場合、Int が x に入る
instance Functor (MyPair c) where
  fmap f (MyPair (x, y)) = MyPair (f x, y)

ghci> newtype MyPair b a = MyPair { getPair :: (a, b) } deriving (Show)
-- MyPair (Int, String) の値を作った場合、MyPair String Int 型の値になる
ghci> let mp = MyPair (1, "AAA")
ghci> :t mp
mp :: Num a => MyPair String a

ghci> fmap (\x -> x + 1000) mp
MyPair {getPair = (1001, "AAA")}
ghci> getPair mp
(1, "AAA")
```

### newtype と遅延評価
`data` と `newtype` は同じようで全く違う。<br>
- data: オリジナルの型を無から作り出すもの
  - パターンマッチ: 何個でも定義できる値コンストラクタから中身を取り出す操作
- newtype: 既存の型を元に、区別される新しい型を作るもの
  - 包んだ型へ変換する操作

以下の例では、パターンマッチによって `undefined` を評価した時にエラーを吐くか、値を返すかが違う。<br>
`data` は値コンストラクタが何個あるかわからないため、`undefined` という値コンストラクタがあるかを探しに行く。<br>
`newtype` は値コンストラクタが1つしかないことを知っているため、渡された値を評価することなく返り値を返す。<br>
```haskell
-- data だとエラーを返す
ghci> data Cool = Cool Bool
ghci> hello :: Cool -> String; hello (Cool _) = "hello!"
ghci> hello undefined
"*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:74:14 in base:GHC.Err
  undefined, called at <interactive>:11:7 in interactive:Ghci7

-- newtype だと値を返す
ghci> newtype Cool2 = Cool2 Bool
ghci> hello2 :: Cool2 -> String; hello2 (Cool2 _) = "hello!"
ghci> hello2 undefined
"hello!"
```

### type, newtype, data を整理
- type
  - 既存の型を別名で使えるようにする
  - 値コンストラクタとかそういうものは一切生じない
- newtype
  - 型クラスのインスタンスを作りやすくするためのもの
  - newtype を使って作った既存の型を包んだ型は、既存の型とは全くの別物
  - 値コンストラクタは1つだけ
  - 値コンストラクタのフィールドは1つだけ
- data
  - 自作の新しい型を作るためのもの
  - 値コンストラクタが複数ある
  - 値コンストラクタのフィールドが複数ある


## Monoid 型クラス
**モノイド**は、結合的な演算と単位元を持つ型のこと。 以下のモノイド則を満たしていないとモノイドではない。<br>
- 結合的な演算: 関数を使って3つ以上の値を1つの値にまとめる計算をする時、計算の順番を変えても結果が変わらないこと
  - *, +, ++ など
- 単位元: 何かの値と結合しても、その値が変わらないこと
  - 0, [], "" など
```haskell
-- 結合的な演算
ghci> 1 + (2 + 3)
ghci> (1 + 2) + 3

ghci [1,2,3] ++ ([4,5] ++ [6])
ghci ([1,2,3] ++ [4,5]) ++ [6]

-- 単位元
ghci> 1 + 0
ghci> 0 + 1

ghci> [1,2,3] ++ []
ghci> [] ++ [1,2,3]
```

**モノイド型クラスの定義**
- mempty: 単位元
- mappend: 結合的な演算
- mconcat: mappend を使ってリストを1つの値にまとめる
```haskell
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```
a mappend b = b mappend a ではないことに注意。<br>

### リストはモノイド
リストは結合的な演算と単位元を持つ。<br>
`Monoid` は具体型を取るため `[a]` になっている。
```haskell
instance Monoid [a] where
  mempty = []
  mappend = (++)

ghci> let empty :: [] String = mempty
ghci> empty
[]

ghci> [1,2,3] `mappend` [5,6,7,10]
[1,2,3,5,6,7,10]

ghci> mconcat [[5,6,7,10], [2,3,4], [10,20,400]]
[5,6,7,10,2,3,4,10,20,400]
```
