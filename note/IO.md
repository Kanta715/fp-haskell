## 入出力

### 基礎
入出力(I/O)の方法を学ぶ前に、まずその基礎を学ぶ。

#### 不純なものと純粋なものを分離する
Haskell は純粋関数型言語であるため、人間からコンピュータに与えるのは一連の実行ステップではなく、あるものが何であるかの定義である。<br>
関数は**副作用**を持つことを許されない。関数は与えられた引数のみを使って何かの結果を返すことしかできない。関数が同じ引数で2回呼ばれた場合は、必ず同じ結果が返される。<br>
このメリットとして、関数が状態を変更できないことにある。グローバル変数などを更新できないことで、型推論などが容易に行えるようになっている。<br>
しかし、サービスを作る上で副作用は切っても切れない関係にある。Haskell には副作用を持つ関数を扱うためのシステムがある。このシステムが、プログラムの純粋(pure)な部分と、画面などとやりとりするような汚い仕事をする不純(impure)な部分をきっちりと分離してくれる（らしい）。<br>
この2つの部分が隔てられているため、外の世界とやりとりしつつも、純粋な部分を推論でしたり、純粋だからこそ遅延評価、堅牢生、関数合成などを利用できる。<br>

以下の型はこのように読める。`putStrLn`は文字列を引数に取り、`()`（空のタプル or unit）を結果とする **I/Oアクション**を返す。<br>
I/Oアクション = 実行される副作用（入力を読んだり、画面やファイルに何かを書き出したり）を含む動作をして結果を返すような何か。このことを「I/Oアクションが結果を生成する」という。<br>
文字列を端末に表示するアクションには実際には意味のある返り値がないため、ダミーの値として`()`を使う。<br>
<div style="background-color: #f5cb42; color: black;">
  NOTE<br>
  putStrLn "A" は、評価されると "A" と表示するのではなく「"A"と表示しろ」という命令書（I/Oアクション）を返す。
</div>

```haskell
-- String を受け取って出力する
ghci> :t putStrLn
putStrLn :: String -> IO ()

-- ユーザーの入力から文字列を生成する
:t getLine
getLine :: IO String
```

I/Oアクションは main という名前をつけて、プログラムを起動すると実行される。<br>
putStrLn, getLine は純粋ではない。（2回実行した時に同じ結果を返す保証がないから。）<br>
I/Oアクションは小さな足のついた関数と考える。外の世界に行って値を取ってきてくれる。外の世界から取ってきた値は、I/Oアクションでしか取り出せない。<br>
`"A" ++ getLine()` みたいな書き方はできない。`<-`を使ってI/Oアクションから値を取得しなければならない。こうやって、pure と impure な関数を分離している。
`getLine(): IO String` なので、配列を連結する `++` 関数は使えない。純粋でないデータを扱う場合は、純粋でない環境の中で行わなければならない。
```haskell
a <- getLine() -- a に束縛する: 不純なものは不純なもの使って取り出す
A ++ a         -- これならOK:   純粋なものとして扱える
```

#### IOアクション同士をまとめる
```haskell
main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey! " ++ name ++ ", you rock!")

Hello, what's your name? -- 出力
Byotaro                  -- 入力
Hey! Byotaro, you rock!  -- 入力値を受け取って出力


-- 適当な関数でIOアクションを合成
module Auth (password) where

password :: IO ()
password = do
  putStrLn "Please typing password"
  pass <- getLine
  let passwords = ["NBPASS", "nbpass", "PASSNB", "passnb"]
  let isActiveUser = pass `elem` passwords
  if (isActiveUser) then putStrLn "Welcome to Haskell World!" else putStrLn "Please enter the correct password."

main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn ("Hey! " ++ name ++ ", you rock!")
  password -- 最後の行以外は、全て変数に束縛可能

Hello, what's your name?
Byotaro
Hey! Byotaro, you rock!
Please typing password
AIUEO
Please enter the correct password.
```
