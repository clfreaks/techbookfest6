# Lem

Lemは、Common Lispで実装されたテキストエディタである。Common Lisp開発でよく用いられる機能がデフォルトで組み込まれており、複雑な設定なしにCommon Lispの開発を始めることができる。

本章では、Lemのインストール方法、SLIMEの使い方、vi-mode等を取り上げながら、Lemを用いたCommon Lispの開発について紹介する。

## インストール

まず、RoswellでLemをインストールする。

```
$ ros install cxxxr/lem
```

Roswellを用いてlemをインストールすると、`lem`コマンドが`~/.roswell/bin`に登録される。次のように`.bashrc`等の設定ファイルでPATHを通しておくと、以降は`lem`コマンドでLemが起動する。

```
export PATH=$PATH:~/.roswell/bin
```

Lemのアップデートは、`ros update`で行うことができる。

```
$ ros update lem
```

## コマンドの入力について

本章では、`C-x`や`M-x`等のコマンド表記が出てくる。それぞれの入力方法は次の通りである。

```
C-c     : Controlキーを押しながらCを押す
M-x     : Metaキー(EscかAltキー)を押しながらXを押す　
C-x o   : Controlキーを押しながらXを押し、Controlキーを離してOを押す
C-x C-e : Controlキーを押しながらXを押し、Controlキーを押したままEを押す
```

## SLIME

SLIMEは`Superior Lisp Interaction Mode for Emacs`の略であり、元はEmacsでCommon Lisp開発を行うためのEmacs Lispプラグインだが、LemではCommon Lispで実装されている。

SLIMEを用いると、SWANKサーバと通信しながら、対話的にアプリケーションを構築することができる。

```
 _______           ____________ 
| SLIME | <-----> | SWANKサーバ |
 ¯¯¯¯¯¯¯           ¯¯¯¯¯¯¯¯¯¯¯¯ 
  　         　  Lispアプリケーション(SBCL等)
```

では、SLIMEの基本的な機能についてみていこう。

### 起動

まず、バッファをlisp-modeにしよう。Lemでは、バッファーをlisp-modeにすると、SWANKサーバが起動する。SWANKサーバが起動すると、下のミニバッファに`Swank server running on sbcl 1.4.14`と表示される。

```
M-x lisp-mode
```

次に、SLIMEを起動しよう。

```
M-x slime
```

SLIMEのREPLが別バッファで起動する。

### コード補完

Lemでは、コード補完がデフォルトで組み込まれている。`lisp-mode`のバッファやREPLでコードを書いている際にTabキーを押すと、シンボル名の候補が表示される。例えば、`defst`と打ち込んだ後にTabキーを押すと、次のような候補が表示される。

```lisp
(defst■______________________________________
      |defstruct                 -- -f---m-- |
      |define-source-context     -- -f---m-- |
      |defsetf                   -- -f---m-- |
      |define-setf-expander      -- -f---m-- |
      |define-hash-table-test    -- -f---m-- |
       ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯
```

### 評価とコンパイル

では、コードのバッファに移動して、関数を定義してみよう。バッファの移動は`M-o`ですることができる。

```lisp
(defun print-test() 
  (print 1))■
```

閉じ括弧の右側にカーソルを合わせて`C-c C-c`とすると、SLIMEがSwankサーバにS式を送りコンパイルする。

SLIMEのREPLに移動後、`print-test`を実行すると、次のような結果になる。

```lisp
CL-USER> (print-test) 

1
1
CL-USER> 
```

SLIMEでSWANKにコードを送りコンパイルした結果、Lispアプリケーションの環境が変化したのが分かる。


### コードジャンプ

`M-.`でシンボルの定義にジャンプ、`M-,`で戻ることができる。例えば、次のフィボナッチ関数の定義と実行コードがあったとする。`(fibonacci 5)`の`fibonacci`の上で`M-.`とすると、フィボナッチ関数の定義にジャンプする。その後、`M-,`とタイプすると、元の位置に戻る。

```lisp
(defun fibonacci (n &optional (a 0) (b 1) (acc ()))
  (if (zerop n)
      (nreverse acc)
      (fibonacci (1- n) b (+ a b) (cons a acc))))

(fibonacci 5)
```

### マクロ展開

`C-c C-m`で1段階のマクロ展開(macroexpand-1)、`C-c M-m`で全段階のマクロ展開をすることができる。マクロ展開をする際は、閉じ括弧ではなく、開き括弧の上でコマンドを打つ必要がある。

```lisp
(defmacro alpha (x y) `(beta ,x ,y))
(defmacro beta (x y) `(gamma ,x ,y))

(alpha 1 2)
;; C-c C-mと打つと、1段階展開された結果が表示される
;;  -> (BETA 1 2) 
;;
;; C-c M-mと打つと、全段階展開された結果が表示される
;;  -> (GAMMA 1 2)
```

### インスペクタ

インスペクタを用いると、シンボルの内部構造を調べたり、シンボル内部の値を変更することができる。シンボルの上にカーソルを合わせて`C-c I`とするとインスペクタが起動する。例えば、Fooクラスからインスタンスhogeを作成して、hogeに対してインスペクタを起動して内部構造をみてみよう。

```lisp
(defclass Foo ()
    ((a :accessor foo-a :initform '(:a :b))
     (b :accessor foo-b :initform :b)))

(setq hoge (make-instance 'foo))
```

```
#<FOO {10055C0623}>
--------------------
Class: #<STANDARD-CLASS COMMON-LISP-USER::FOO>
--------------------
Group slots by inheritance [ ]
Sort slots alphabetically  [X]

All Slots:
[ ]  A = (:A :B)
[ ]  B = :B

[set value]  [make unbound]
```

`All Slots:`のAの上でReturnを押すと、スロットAの内部構造を見ることができる。

```
#<CONS {100574BCB7}>
--------------------
A proper list:
0: #<SB-MOP:STANDARD-DIRECT-SLOT-DEFINITION COMMON-LISP-USER::A>
1: #<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION COMMON-LISP-USER::A>
```

また、インスタンスのスロットの値を変更することができる。スロットの左にある[　]にチェックをつけて[set value]を押すと、ミニバッファ上で値を変更できる。

```
#<FOO {10055C0623}>
--------------------
Class: #<STANDARD-CLASS COMMON-LISP-USER::FOO>
--------------------
Group slots by inheritance [ ]
Sort slots alphabetically  [X]

All Slots:
[ ]  A = (:A :B)
[X]  B = :B

[set value]  [make unbound]
```

ここでは、スロットBの値に`'(:C :D)`を再設定する。

```
Set slot B to (evaluated) : '(:C :D) 
```

インスタンスのスロットBの値が変更されたのがわかる。

```
All Slots:
[ ]  A = (:A :B)
[X]  B = (:C :D)
```

### デバッガ

SLIMEには、SLDB(SLIME debugger)というデバッガが入っている。ここでは、リスタートとブレークポイントを用いたデバッグ方法を紹介する。

#### リスタート

リスタートの機能を用いると、エラーに対して操作を加えながら、コードの動作を試すことができる。

次のような関数を定義したとする。関数本体に束縛されていない変数yが存在するために、関数実行時にエラーが発生する。

```
(defun f1(x)
 (+ x y))
```

REPLに移動して、`(f1 1)`と関数を実行すると、次のようなエラーが発生する。

```
CL-USER> (f1 1) 
```

```
The variable Y is unbound.
   [Condition of type UNBOUND-VARIABLE]

Restarts:
 0: [CONTINUE] Retry using Y.
 1: [USE-VALUE] Use specified value.
 2: [STORE-VALUE] Set specified value and use it.
 3: [RETRY] Retry SLIME REPL evaluation request.
 4: [*ABORT] Return to SLIME's top level.
 5: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1001AB8413}>
```
ここでは、Restartsで`2: [STORE-VALUE]`を選択して、Yに値を設定して再実行する。

```
CL-USER> (f1 2) 
Type a form to be evaluated: 4 
 
6
```

すると、関数を実行して`6`という評価値を得ることができる。

#### ブレークポイント

次のフィボナッチ関数の定義では、停止条件が合致した際に整数をゼロで割るとエラーが発生する。

```lisp
(defun fib(n)
  (if (<= 0 n 1)
      (/ 1 0)
      (+ (fib (- n 1))
         (fib (- n 2)))))
```

このような場合、関数の最初に`(break)`を挿入すると、1段階ずつトレースができるようにする。


```lisp
(defun fib(n)
  (break)
  (if (<= 0 n 1)
      (/ 1 0)
      (+ (fib (- n 1))
         (fib (- n 2)))))
```

`(break)`を挿入後、関数を再度コンパイルして、次のように関数を実行する。

```
(fib 9)
```

`Restarts`で`0: [Continue]`を選択しながら、スタックをトレースしていく。`Backtrace:`の各スタックにカーソルを合わせてReturnを押すと、スタックの状態を確認することができる。例えば、`(FIB 5)`を選択することで、引数Nの値が5であることが確認できる。

```
break
   [Condition of type SIMPLE-CONDITION]

Restarts:
 0: [CONTINUE] Return from BREAK.
 1: [RETRY] Retry SLIME REPL evaluation request.
 2: [*ABORT] Return to SLIME's top level.
 3: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1001AB041
 
Backtrace:
  0: (FIB 5)
       Locals:
         N = 5
  1: (FIB 6)
  2: (FIB 7)
  3: (FIB 8)
  4: (FIB 9)
```

スタックが`(FIB 1)`まで来た時に、`DIVISION-BY-ZERO`エラーが起こるのが確認できる。

```
arithmetic error DIVISION-BY-ZERO signalled
Operation was (/ 1 0).
   [Condition of type DIVISION-BY-ZERO]

Restarts:
 0: [ABORT] Abort compilation.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "worker" RUNNING {10025C9153}>)

Backtrace:
  0: (SB-KERNEL::INTEGER-/-INTEGER 1 0)
  1: (FIB 2)
  2: (FIB 3)
```

## キーバインド

デフォルトのキーバインドは、Emacsのものとほぼ同じである。基本的なキーバインドは次の通りである。

```
# カーソル移動
C-p: １行上に移動　　　C-n: 1 行下に移動　　C-f: 1 文字前に移動　　C-b: 1 文字後に移動
C-a: 行の先頭に移動　　C-e: 行の末尾に移動
M-f: １単語前に移動　　M-b: 1 単語後に移動
C-v: １画面下に移動　　M-v: 1 画面上に移動
M-<: バッファの先頭に移動　　M->: バッファの末尾に移動
C-g: 現在の操作を終了

# ファイル操作系
C-x C-f：ファイルを開く
C-x C-s：現在のバッファをディスクに保存
C-x C-w：現在のバッファを新しいファイル名で保存
C-x i：ファイルの挿入
C-d：ディレクトリを開く

# 編集
C-_：最後の操作を取り消す
C-s：文字列を前方検索　　C-r：文字列を後方検索
C-d：カーソルの文字を削除　　C-h：カーソルの一文字前を削除
M-d：カーソル位置の単語をカット
C-k：カーソルの位置から行末まで削除(カット)
C-y：貼り付け
C-SPC：マークセット
C-w：マーク部分からカーソル位置までを切り取り
M-w: マーク部分からカーソル位置までをコピー
C-x h：全範囲選択

# バッファの移動
C-x o: 次のバッファーに移動
C-x C-b: バッファ一覧を表示

# ウィンドウ分割
C-x 2：2行に分割　　C-x 3: 2列に分割
C-x 0：カーソルのあるほうの分割したウィンドウを閉じる
C-x 1：カーソルのないほうの分割したウィンドウを閉じる

# 矩形選択
`C-x Space`で矩形選択をすることができる。
```

## vi-mode

### 初期設定

次は、vi-modeについて見ていこう。

Lemでvi-modeを利用するには、`~/.lem/init.lisp`に次の1行を加えるか、もしくは、`M-x vi-mode`とする。

```
(lem-vi-mode:vi-mode)
```

### モードの切り替え

Lemのvi-modeでは、viと同様、コマンドモードへは`esc`キー、インサートモードへは`i`、ビジュアルモードには`v`で切り替える。

基本的なコマンドは次の通りである。

```
# カーソル移動
h, j, k, l - 左,下,上,右に移動
w - 単語ごとに1文字目に移動(句読点で停止)　　　  W - 単語ごとに1文字目に移動(句読点を無視)
e - 単語ごとに最後の文字に移動(句読点で停止)　 　E - 単語ごとに最後の文字に移動(句読点を無視)
b - 前の単語ごとに1文字目に移動(句読点で停止)　  B - 前の単語ごとに1文字目に移動(句読点を無視)
0 - 行の最初に移動　　^ - 行の最初の文字に移動 　$ - 行の最後に移動
G - 指定の行に移動(5Gは5行目に移動)

# テキストの挿入
i - インサートモードを開始　　　I - 行の始めでインサート
a - カーソルの後でインサート　　A - 行の最後でインサート
o - カーソルの場所で改行　　　　O - カーソル下に空白の行を挿入
ea - 単語の最後の文字でインサート
Esc - インサートモードを終了

# テキスト編集
r - カーソルの文字を置き換える(インサートモードに入らない)　　
J - 1行下の行を現在の行の最後に移動する
cc - 行を消して置き換える　　　cw - 単語を消す　　c$ - 行を消す
s - カーソル位置の文字を消す　 S - 行を消して置き換える
xp - カーソルの文字を1つ右に移動する
u - 1つ前の操作を取り消す　　　C-r - 1つ前の操作を繰り返す

# ビジュアルモード
v - 文字単位でビジュアルモードを開始　　V - 行単位でビジュアルモードを開始
Ctrl+v - 矩形選択(ブロック選択)を開始
Esc - ビジュアルモードを終了

# カット・ペースト
yy - 1行分コピー　　yw - 単語をコピー
p - カーソルの後にクリップボードをペースト
P - カーソルの前にクリップボードをペースト
dd - 1行削除(カット)　　dw - 現在地の単語を削除(カット)
x - 現在地の文字を削除(カット)

# 終了
:w - ファイルを保存　　:wq - ファイルを保存して、エディタを終了
:q - エディタを終了する(ファイルに変更があった場合は失敗する)
:q! - エディタを強制終了

# 検索・置換
/pattern - パターン検索　　　 ?pattern - 後方へのパターン検索
n - 検索を同じ方向に繰り返す　　N - 検索を逆方向に繰り返す
:%s/old/new/g - ファイル内全てののoldをnewに置き換え
:%s/old/new/gc - ファイル内全てののoldをnewに置き換え(確認あり)

# 複数のファイルへの操作
:e filename - 新しいバッファでファイルを編集
:sp filename - 新しいファイルを新しいバッファで開き、画面を分割
:bn - 次のバッファに移動
:bp - 前のバッファに移動
:bd - バッファを削除(ファイルを閉じる)
C-ws - 画面を分割　　　　C-wv - 画面を垂直方向に分割
C-ww - 画面の切り替え　　C-wq - 画面を閉じる
```

## 拡張の書き方

