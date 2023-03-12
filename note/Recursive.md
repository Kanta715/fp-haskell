## 再帰
再帰とは、関数の定義の中で自分自身を呼び出す関数の定義の仕方。<br>
Haskell では計算を**どうやって**するのかを指定するのではなく、求めるものが**何であるか**を宣言して計算を行う。
Haskell の目的は、計算を実行するステップをコンピュータに示すとこではなく、欲しい結果が何であるかを直接定義することである（らしい）

maximum 関数で考えてみる。maximum 関数は  順序の付いた値のリストを受け取り、その中で一番大きな値を返す。<br>
まず、再帰的な関数の前に、命令的に定義した場合を考える。<br>
最大値を保持するための可変の変数を定義し、リストの全ての要素をループして、ループしている要素がこれまでの最大値よりも大きければ最大値を更新していく。ループ終了時の変数がリスト内の最大値となる（Scala で書いてみる）
```scala
def maximum(ints: Seq[Int]): Int = {
  var max = 0
  ints.foreach(int => {
    if (max < int) max = int
  })
  max
}

scala> maximum(Seq(1,2,3,3,4,4,4,4,4,4,4,9))
res0: Int = 9
```

次に、再起的に定義する方法を見ていく。<br>
最初に基底部を定義する。単一要素のリストに対する最大値は、その唯一の要素と同じ値。もっと、要素がある場合は、リストの先頭要素と残りのリストの最大値とでどちらが大きいかを調べる。<br>
配列の要素が2つ以上ある場合は、先頭要素以外の要素で再帰的に maximum を呼ぶ。
```haskell
maximum' :: (Ord o) => [o] -> o
maximum' []     = error "Error: Empty list!"
maximum' [o]    = o
maximum' (o:ol) = max o (maximum' ol)

ghci> maximum' [1,1,1,1,1,1,2,2,2,2,3,3,3,3,3,3,3,3,5,5,56,6,7,7,200,8,98,9,9,9]
200

-- Image
-- maximum' [2,5,1] =
--   max 2 ( maximum' [5,1] =
--     max 5 ( maximum' [1] = 1 )
--   )
```

### 他の再帰関数を実装してみる

##### replicate
Int と値を受け取り、その値を指定された数だけ複製する `replicate` を実装。<br>
まず、基底部を考える。<br>
- 0 or 0以下の回数だけ複製しろと要求された場合
  - 空のリストを返す
- それ以外の場合
  - 値 x を n 回繰り返したリストを返す
  - ロジック: x を head に、n -1 回繰り返したリストを tail に構築したリスト
```haskell
replicate' :: Int -> a -> [a]
replicate' int val
 | int <= 0 = []
 | otherwise = val : replicate' (int - 1) val

ghci> replicate 5 "LATTE"
["LATTE","LATTE","LATTE","LATTE","LATTE"]
```

##### take
リストから指定された数の値のリストを返す。<br>
基底部は2点
- 0以下の数が指定された場合は空のリスト返すこと
- 空のリストが渡された場合は、空のリストを返すこと
```haskell
take' :: Int -> [a] -> [a]
take' int _
  | int <= 0          = []
take' _ []            = []
take' int (head:tail) = head : take' (int - 1) tail

ghci> take' 2 [1,3,45,7,8,9,9]
[1,3]
```

##### reverse
リストを引数に取り、要素の並びを逆順にして返す関数<br>
基底部
- 引数が空のリストの場合、空のリストを返す
```haskell
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (head:tail) = reverse' tail ++ [head]

ghci> reverse [1,3,45,7,8,9,9]
[9,9,8,7,45,3,1]
```

##### repeat
無限リスト作る関数
```haskell
repeat' :: a -> [a]
repeat' a = a : repeat' a

repeat' 2
[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2...
```

##### zip
2つのリストを引数に取り、これらを綴り合わせる。<br>
[1,3,5], [1,2] の場合、[(1,1), (3,2)] を返す<br>
基底部
- 空の場合は空を返す
```haskell
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (h:t) (h2:t2) = [(h, h2)] ++ zip' t t2

ghci> zip' [1..2000] ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"]
[(1,"One"),(2,"Two"),(3,"Three"),(4,"Four"),(5,"Five"),(6,"Six"),(7,"Seven"),(8,"Eight"),(9,"Nine"),(10,"Ten")]
```

#### elem
値とリストを受け取り、その値がリストに含まれるかどうか。
基底部
- 空のリストの場合、False を返す
```haskell
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' v (head:tail)
  | v == head = True
  | otherwise = elem' v tail

ghci> elem' 1 [1..100]
True
ghci> elem' 1 [2..100]
False
```

## クイックソート
ポビット（軸）を中心として並べ替えていく
```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort []          = []
quicksort [a]         = [a]
quicksort (head:tail) =
  let
    smallOrEqual = [s | s <- tail, s <= head]
    large        = [l | l <-   tail, l  > head]
  in
    quicksort smallOrEqual ++ [head] ++ quicksort large

ghci> quicksort [200, 1000, 2, 1, 50, 199, 51]
[1,2,50,51,199,200,1000]
```

## 再帰的に考える
1. 基底部を考える
2. 問題を1つもしくはそれ以上の部分問題に分解し、自分自身を適用することによって、それらの部分問題を再帰的に解く
3. 最終的な解を部分問題の解から構築する

例：ソートの場合
1. 空リストの場合を考える
2. リストを2つのリストとピボットに分解すし、それらのリストを再帰的にソートする
3. 結果が得られたら、結合してソート済みのリストとして返す
