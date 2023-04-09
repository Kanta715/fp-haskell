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
data Func from to = Func (from -> to)

instance Functor (Func from) where
  fmap f (Func g) = Func (\x -> f (g x))

plus10Com :: Func Int Int
plus10Com = Func plus10

plus10 :: Int -> Int
plus10 n = n + 10

toString :: (Show a) => a -> String
toString a = show a


ghci> let c = fmap toString plus10Com
ghci> :t c
c :: Func Int String

-- Func Int String から 関数の部分を取り出す
ghci> let Func f = c

ghci> f 10
"20"
```

`fmap` は、関数とファンクター値を取って、ファンクター値を返す2引数関数とも思えるが、関数を取って「ファンクター値を取ってファンクター値を返す関数」を返す関数だと思うこともできる。<br>
関数 `a -> b` を取って、関数 `f a -> f b` を返す。こういう操作を関数の持ち上げ（lifting）という。<br>
関数で `Functor` 値を写すこと = 関数を持ち上げてからファンクター値に適用
```haskell
ghci> let l = fmap toString
ghci> :t l
l :: (Functor f, Show a) => f a -> f String
```
