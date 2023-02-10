# 関数型プログラミングを Haskell で学ぶ
業務、個人開発で Haskell を書くことはないため初めて触るが、Scala の本の難易度がかなり高かったため会社の先輩がお薦めしていた Haskell の本で学習を進めることにした。
<br>
今回の目標は、関数型プログラミングで書くことの旨味を自分なりに理解できることとする。

## Stack
```
$> stack new <project>
// ビルド
$> stack build
// 対話型
$> stack repl
$> stack ghci 
```

## Haskell 基本構文
すぐ忘れるからメモ
```haskell
-- 型宣言
str :: String

{- 関数定義
 1. 関数の型宣言(-> で引数と戻り値を繋ぐ)
 2. 関数の具体的な処理を宣言
-}
printLine :: String -> IO()
printLine x = putStrLn x

-- モジュールのエクスポート
module X(x) where
-- xという関数定義
-- x ...

-- モジュールのインポート
import X
```
