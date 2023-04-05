## もっと入出力
ファイルとのやりとりなどを行う。<br>
ストリーム = 時間をかけてプログラムに出たり入ったりする連続したデータ片のこと。（例：キーボード入力時の文字など）<br>

### 入力のリダイレクト
テキストファイルの内容をプログラムに入力として与える。<br>
入力のリダイレクトは `{実行コマンド} < {ファイル名}` の形で行う
```haskell
-- 入力を永遠に受け取る forever 関数
main = forever $ do
  str <- getLine
  putStrLn . map toUpper $ str

-- 入力のリダイレクトは < {ファイル名} の形で行う
$ stack exec result < io/fileio.txt
FILEIO-TEST

TEST-FILEIO

UPPER-CASE

GEORGIA COFFEE

NB-NB
```

`forever` は1行ずつ都度、入力を読み込む
```haskell
main = forever $ do
  str <- getLine
  putStrLn str
  putStrLn . map toUpper $ str

fileio-test
FILEIO-TEST


test-fileio
TEST-FILEIO


upper-case
UPPER-CASE


georgia coffee
GEORGIA COFFEE


nb-nb
NB-NB
```
<br>

`getContents` は EOF まで全ての入力を読み込む。<br>
`getContents` は遅延I/Oを行うため、必要になった時だけ入力から値を読み込み、`contents` に束縛する。<br>
`getContents` の結果が `contents` に束縛される時、それは本当の文字列ではなく、最終的に文字列として評価されるプロミス（promise）としてメモリに置かれる。
`contents` に `toUpper` をマップするときも入力の結果に関数をマップするというプロミスになる。
最終的に `putSrtLn` が呼ばれると、これがプロミスに対して「大文字にされた行」を要求する。そのプロミスはまだ入力の行を持っていないため、`contents` に対し端末の入力を問い合わせる。
そこで初めて `getContents` が実際に端末から入力された文字列を読み込み、`contents` に束縛される。<br>
<br>
**処理の流れ**
1. `getContents` は入力を読み込むプロミスを作成する
2. `contents` に `toUpper` をマップするプロミスを作成する
3. `putStrLn` がプロミスに対して「大文字にされた行」を要求する
4. `contents` に対し端末の入力を問い合わせる
5. `getContents` が実際に端末から入力された文字列を読み込み、`contents` に束縛される
```haskell
main = do
  contents <- getContents
  putStrLn . map toUpper $ contents

fileio-test

test-fileio

upper-case

georgia coffee

nb-nb

FILEIO-TEST

TEST-FILEIO

UPPER-CASE

GEORGIA COFFEE

NB-NB
```

### ファイルの読み書き
ファイルの読み書きは `System.IO` モジュールを使う。<br>
```haskell
main :: IO ()
main = fileIOFunc

fileIOFunc :: IO ()
fileIOFunc = do
  handle   <- openFile "io/fileio2.txt" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

$stack run
大谷＞はじめまして。私は大谷翔平です。

藤浪＞はじめまして。私は藤浪晋太郎です。

大谷＞野球スキルを教えてください。

藤浪＞投手です。最速162km/hのストレートとスライダーが得意な球種です。

大谷＞投手ですか。私は投手と野手です。

大谷＞最速165km/hのストレートとスライダー、スプリットが得意な球種です。
```

**処理の流れ**<br>
**1**<br>
`openFile` はファイルを開く関数で以下の型シグネチャを持つ。<br>
`FilePath` は `String` の型シノニム（別名）で、ファイルのパスを表す。<br>
```haskell
ghci> :t openFile
openFile :: FilePath -> IOMode -> IO Handle
```
**2**<br>
`IOMode` は `ReadMode` か `WriteMode` か `AppendMode` のいずれかで、ファイルを読み込みモードで開くか書き込みモードで開くか追記モードで開くかを表す。<br>
この型は開いたファイルに対して何をしたいのか列挙型。* `IOMode` は `IO Mode` ではない。`IO Mode` だと `Mode` 型を生成する I/O アクションを意味する。<br>
```haskell
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode 
```
**3**<br>
openFile は指定されたファイルを指定されたモードで開く I/O アクションを返す。<br>
そのアクションの結果を束縛することで、そのファイルに対する Handle が得られる。<br>

**4**<br>
`hGetContents` はコンテンツをどのファイルから読みだすべきか知っている Handle を受け取り、そのファイルに含まれる内容を結果として返す。<br>
`getContents` と `hGetContents` は同じように動作するが、`hGetContens` は渡された Handle から入力する。それ以外は全て同じ。<br>
`getContents` と同じように、`hGetContents` は遅延I/Oを行うため、必要になった時だけ入力から値を読み込み、`contents` に束縛する。<br>
```haskell
ghci> :t hGetContents
hGetContents :: Handle -> IO String
```

**5**<br>
`handle   = ファイルを指し示すポインタ`<br>
`contents = ファイル内の値`<br>
`contents` に対し `putStr` を適用すると、`contents` に対するプロミスが評価され、そのプロミスがファイルの内容を返す。<br>
`hClose` はファイルを閉じる関数で、`openFile` と対になる関数。<br>
閉じたいファイルを指し示す Handle を受け取り、そのファイルを閉じる I/O アクションを返す。<br>
```haskell
ghci> :t hClose
hClose :: Handle -> IO ()
```

#### withFile 関数
withFile 関数はファイルを開き、そのファイルに対する処理を行い、ファイルを閉じるという一連の処理を行う関数。<br>
パス、モード、ファイルに対する処理を受け取り、その処理を行う I/O アクションを返す。<br>
withFile 関数は、ファイルを開くときにエラーが発生した場合でも確実にハンドルを閉じてくれる。<br>
同じ処理でも withFile を使うと、より簡潔に書ける。（勝手にハンドルを閉じてくれる）<br>
```haskell
ghci> :t withFile
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

fileIOFunc2 :: IO ()
fileIOFunc2 = do
  withFile "io/fileio2.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStr contents
```

NOTE<br>
以下の関数を使えば Handle を閉じるのを Haskell が自動でやってくれる。
- readFile 関数
- writeFile 関数
- appendFile 関数
