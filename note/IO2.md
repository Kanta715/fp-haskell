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
