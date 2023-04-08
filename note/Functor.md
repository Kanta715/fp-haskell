## ファンクター
`Functor` 型クラスは `fmap` を定義している。<br>
`fmap` は a -> b の関数、 f a の値を受け取り、f b にして返す。（a 型に関数を適用して返しくれる）<br>

ファンクターは、文脈を持った値だとみなすことができる。<br>
例えば、`Maybe` 型は、値が存在するかしないかを表し、リストの場合は、値が複数存在するかもしれないという文脈を表す。`fmap`はこういった文脈を保ったまま関数を値に適用する。<br>
```haskell
fmap :: (a -> b) -> f a -> f b
```

### 関数ファンクター
`->`を用いて関数を表すがこれもファンクター。<br>
`->` は2つの型引数を取る型コンストラクタだが、Functor のインスタンスになるためには、1つの型引数しか受け取れない。<br>
なので `(->) r` のように、1つの型引数を取っている部分適用している。
```haskell
ghci> :k (->)
(->) :: * -> * -> *

Functor ((->) r)
  fmap :: (a -> b) -> (r -> a) -> r -> b
```

まず `Functor` 型クラスの実装を思い出す。<br>
`fmap` 関数は `a -> b` の関数を受け取り、`Functor a` を受け取って、`Functor b` にして返す関数である。
```haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

これを`(->)`インスタンスの `fmap` で考える。<br>
`Functor` 型クラスの `f` が `(->) r`になる。
```haskell
-- f を (->) r で置き換える 
fmap :: (a -> b) -> ((->) r a) -> ((->) r b)

-- 中置記法で見やすくする
fmap :: (a -> b) -> (r -> a) -> (r -> b)
```

`(->)`インスタンスのやっていること。
1. a -> b の関数を取る: a 型の値を b型の値に変換する関数を受け取る
2. r -> a の関数を受け取る: r 型の値を a 型に変換する関数を受け取る
3. r -> b の関数を返す: r 型の関数を受け取って、b 型ににして返す関数を返す

このことから `(->)`インスタンスは関数を合成していることがわかる。

**独自の関数合成型**<br>
`->` と同じように、Functor インスタンスにして `fmap` で関数合成を再現。
```haskell
data Composite from to = Composite (from -> to)

instance Functor (Composite from) where
  fmap f (Composite g) = Composite (\x -> f (g x))

plus10Com :: Composite Int Int
plus10Com = Composite plus10

plus10 :: Int -> Int
plus10 n = n + 10

toString :: (Show a) => a -> String
toString a = show a


ghci> let c = fmap toString plus10Com
ghci> :t c
c :: Composite Int String
```
