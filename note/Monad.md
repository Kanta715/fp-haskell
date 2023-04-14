## モナド
Monad は Applicative の強化版と言える。（Applicative は Functor の強化版）<br>

**これまでのおさらい**
- Functor: 関数を値に適用する
- Applicative: 文脈を維持したまま値に関数を適用する

## Monad 型クラス
**関数**
- `(>>=)`: モナド値を取り出して関数に渡し、その結果をモナドに入れる
  - この関数が Monad にする意味を見出す
  - `m a`: モナド値
  - `(a -> m b)`: モナド値を取り出して関数に渡し、その結果をモナドに入れる
  - `m a` は何らかの文脈を持った値である。その文脈を踏まえて関数を適用することで、文脈を維持したまま値に関数を適用することができる
  - 文脈付きの `m a` に依存して `(a -> m b)` を適用できる
  - Applicative (<*>) 前の値の文脈に依存して処理をするのが苦手
- `(>>)`: 2つのモナド値を取って、最初のモナド値を無視して、2つの目のモナド値を返す（なんでいるんやこれ）
- `return`: 値をモナドに入れる（Applicative の pure）
```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

### MyMaybe を作る
```haskell
data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq, Ord, Read)

instance Functor MyMaybe where
  fmap f MyNothing  = MyNothing
  fmap f (MyJust x) = MyJust (f x)

instance Applicative MyMaybe where
  pure = return

  MyNothing <*> _         = MyNothing
  _         <*> MyNothing = MyNothing
  MyJust f  <*> MyJust x  = MyJust (f x)

instance Monad MyMaybe where
  return x = MyJust x

  MyNothing >>= f = MyNothing
  MyJust x  >>= f = f x

getPlus100MyMaybe :: Int -> MyMaybe Int
getPlus100MyMaybe x = return (x + 100)

---------------------------------------------------------------------------------

ghci> let myMonad :: MyMonad Int = return 100
ghci> mm
MyJust 100

-- 文脈は失われない
ghci> mm >>= getPlus100MyMaybe
MyJust 200

ghci> MyNothing >>= getPlus100MyMaybe
MyNothing
```

### do 記法
今までの学習では do 記法は IO アクションを1つのまとめるために使ってきた。<br>
しかし do 記法は Monad 型クラスのインスタンスである型に対して使うことができる。<br>
do 記法は複数の Monad 値を糊付けするものである。<br>
