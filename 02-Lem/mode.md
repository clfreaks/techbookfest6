# Lem

Lemは、Common Lispでの開発に必要な機能が一通り組み込まれているエディタです。入力補完機能や、Common Lisp処理系とのやりとりを行う機能(SLIME)などがデフォルトで組み込まれているため、インストール後すぐに開発を始めることができます。また、Lem自体もCommon Lispで実装されており、Common Lispを使って拡張機能を作ることができます。
 
本章では、Lemのインストール、基本的な操作法、SLIMEの使い方、vi-mode、拡張機能の書き方を紹介します。基本的な操作法はEmacsと共通しているので、すでにEmacsでSLIMEの利用経験がある方は、拡張機能の節から読み進めても構いません。
 
## Lemのインストール

Roswellを使ってLemをインストールします。

```
$ ros install cxxxr/lem
```

このようにLemをインストールすると、Lemの実行ファイルが`~/.roswell/bin`にできます。次のように`.bashrc`等の設定ファイルで`PATH`環境変数を設定しておくと、以降は`lem`コマンドで起動できるようになります。


```
export PATH=$PATH:~/.roswell/bin
```

`lem`コマンドにファイル名を引数として与えると、そのファイルを直接開いて編集することができます。また、引数にディレクトリを指定することで、ファイラ(dired)を開いた状態でLemを起動することができます。

```
# ファイル名を指定して開く
$ lem <ファイル名>

# diredモードで開く
$ lem <ディレクトリ名>
```

```
/home/t-cool/.roswell/

      7k 2019/02/15 13:59:19 Fri ..
     352 2019/02/11 21:45:49 Mon archives/
     288 2019/02/15 13:59:17 Fri bin/
      96 2018/12/02 15:28:08 Sun env/
     128 2018/12/02 15:27:29 Sun impls/
      96 2018/12/02 15:38:29 Sun lib/
```

### Lemの更新
Lemを最新の状態にするには、以下のように`ros update lem`を実行します。

```
$ ros update lem
```

## コマンド表記と入力方法について

本章では、`C-x`や`M-x`等のコマンド表記が出てきますが、それぞれの入力方法は次の表の通りです。

| コマンド表記 | 実際の入力方法 |
|:-----------:|:------------:|
| `C-c` | `Control`キーを押しながら`C`を押す |
| `M-x` | `Meta`キー(`Esc`か`Alt`キーであることが多い)を押しながら`X`を押す |
| `C-x C-e` | `Control`キーを押しながら`X`を押し、そのまま`Control`キーを押したまま`E`を押す |
| `C-x o` | `Control`キーを押しながら`X`を押し、`Control`キーを離してから`O`を押す |


## Lemの起動と終了

`~/.roswell/bin`にPATHが通っていると、次のようにターミナルでファイルを作成してLemを起動できます。

```
$ lem <ファイル名.lisp>
```

`C-x C-s`でファイルへの保存が終了後、`C-x C-c`でLemを終了することができます。

## SLIME

SLIMEは`Superior Lisp Interaction Mode for Emacs`の略であり、元はEmacsでCommon Lisp開発を行うためのEmacs Lispプラグインです。LemではCommon Lispで実装されています。SLIMEを使うと、下のようにSWANKサーバを介して対話的にLispアプリケーションを構築することができます。SLIMEは、SWANKサーバに式を送り評価やコンパイルのリクエストを出したり、SWANKサーバはLispアプリケーションの状態をSLIMEに伝えたりします。
 
では、SLIMEの基本的な使い方をみていきましょう。

### SLIMEの起動

まず、バッファをlisp-modeにします。Lemでは、バッファをlisp-modeにするとSWANKサーバが起動します。SWANKサーバが起動すると、下のミニバッファに`Swank server running on sbcl 1.4.14`と表示されます。

```
M-x lisp-mode
```

次に、`M-x slime`でSLIMEを起動すると、SLIMEのREPLが別バッファで起動します。

```
M-x slime
```

```
CL-USER>  
```

### REPL

試しにREPLに式を打ち込んでみます。

```
CL-USER> (cons 'a '(b c))
(A B C)
CL-USER> (mapcar #'1+ '(1 2 3)) 
(2 3 4)
```

`M-p`でREPLの履歴を確認できます。

```
CL-USER> (cons 'a '(b c))
```

### 入力補完

Lemでは、入力補完がデフォルトで組み込まれています。`lisp-mode`のバッファやREPLでTabキーを押すと、シンボル名の候補が表示されます。例えば`defst`と打ち込んだ後にTabキーを押すと、次のように候補が表示されます。

```lisp
(defst 
    defstruct                      -- -f---m-- 
    define-source-context          -- -f---m-- 
    defsetf                        -- -f---m-- 
    define-setf-expander           -- -f---m-- 
```    

また、SLIME上でライブラリを読み込むことで、そのライブラリで定義されている関数やマクロを探ることができます。exportされているシンボルは、1つコロンをつけると参照することができます。
 
 ```
CL-USER> (ql:quickload :cl-ppcre) 
To load "cl-ppcre":
  Load 1 ASDF system:
    cl-ppcre
; Loading "cl-ppcre"
.
(:CL-PPCRE)
CL-USER> (cl-ppcre: 
                cl-ppcre:*allow-named-registers*        -- b-------
                cl-ppcre:*allow-quoting*                -- b-------
                cl-ppcre:*look-ahead-for-suffix*        -- b-------
                cl-ppcre:*optimize-char-classes*        -- b-------
```

exportされていないシンボルも、2つコロンをつけると参照できます。

```
CL-USER> (cl-ppcre:: 
cl-ppcre::                              -- -------p
cl-ppcre-asd::                          -- -------p
cl-ppcre::%add-to-charset               -- -f------
cl-ppcre::%add-to-charset/expand        -- -f------
cl-ppcre::%temp                         -- --------
```

### 評価とコンパイル

コードのバッファに移動して、関数を定義してみます。`M-o`でバッファ間を移動できます。

```lisp
(defun print-test() 
  (print 1))
```

閉じ括弧の右側にカーソルを合わせて`C-c C-c`とすると、SLIMEがSWANKサーバにS式を送り、評価とコンパイルを行います。

SLIMEのREPLに移動後、`print-test`を実行すると、次のような結果になります。

```lisp
CL-USER> (print-test) 

1
1
CL-USER> 
```

### コードジャンプ

`M-.`でシンボルの定義にジャンプ、`M-,`で戻ることができます。
  
Lemのソースコード中でコードジャンプをするには、まずREPLでLemのシステムを読み込む必要があります。
 
 ```
CL-USER> (ql:quickload :lem)
To load "lem":
  Load 1 ASDF system:
   lem
 ; Loading "lem"
.......
(:LEM)
 ```
 
システムの読み込みが完了すると、Lemのソースコード内で定義場所にジャンプできるようになります。 例えば、Lemのxml-mode(`lem/modes/xml-mode/xml-mode.lisp`)で使われている`define-major-mode`の定義場所を探ってみます。

```
(define-major-mode xml-mode language-mode
     (:name "xml"
      :keymap *xml-mode-keymap*
      :syntax-table *xml-syntax-table*
      :mode-hook *xml-mode-hook*)
   (setf (variable-value 'enable-syntax-highlight) t
         (variable-value 'tab-width) 2
         (variable-value 'calc-indent-function) 'xml-calc-indent))
```

`define-major-mode`の上で`M-.`とすると、マクロの定義場所にジャンプすることができます。`C-x C-f`でファイルの場所を調べると、`lem/lib/core/mode.lisp`で定義されているのが分かります。
  
```
(defmacro define-major-mode (major-mode
                             parent-mode
                             (&key name keymap syntax-table mode-hook)
                             &body body)
; 以下、省略                             
)                              
```

定義の確認後、`M-,`で元の場所に戻ることができます。 

### マクロ展開

`C-c C-m`で1段階のマクロ展開(macroexpand-1)、`C-c M-m`で全段階のマクロ展開をすることができます。マクロ展開をする際は、閉じ括弧ではなく、開き括弧の上でコマンドを打つ必要があります。

```lisp
(defmacro alpha (x y) `(beta ,x ,y))
(defmacro beta (x y) `(gamma ,x ,y))

(alpha 1 2)
```

上の例で`(alpha 1 2)`に対してマクロ展開をする場合、`C-c C-m`では1段階展開された結果`(BETA 1 2)`、`C-c M-m`では全段階展開された結果`(GAMMA 1 2)`が表示されます。

### インスペクタ

インスペクタを用いると、シンボルの内部構造を調べたり、シンボル内部の値を変更することができます。シンボルの上にカーソルを合わせて`C-c I`とするとインスペクタが起動します。Fooクラスからインスタンスhogeを作成して、hogeに対してインスペクタを起動して内部構造をみてみましょう。

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

`All Slots:`の`A`の上でReturnを押すと、スロット`A`の内部構造を見ることができます。

```
#<CONS {100574BCB7}>
--------------------
A proper list:
0: #<SB-MOP:STANDARD-DIRECT-SLOT-DEFINITION COMMON-LISP-USER::A>
1: #<SB-MOP:STANDARD-EFFECTIVE-SLOT-DEFINITION COMMON-LISP-USER::A>
```

また、インスタンスのスロットの値を変更することができます。スロットの左にある[　]にチェックをつけて[set value]を押すと、ミニバッファ上で値を変更できます。

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

ここでは、スロット`B`の値に`'(:C :D)`を再設定します。

```
Set slot B to (evaluated) : '(:C :D) 
```

```
All Slots:
[ ]  A = (:A :B)
[X]  B = (:C :D)
```

インスタンスのスロット`B`の値が変更されたのがわかります。

### デバッガ

SLDB(SLIME debugger)を用いたデバッグ方法を紹介します。

#### リスタート

仮に、次のような関数を定義したとします。関数本体に束縛されていない変数`y`が存在するために、実行時にエラーが発生することが予想されます。

```
(defun f1(x)
 (+ x y))
```

REPLに移動して、`(f1 1)`と関数を実行すると、エラーが発生して、SLDBが起動します。

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

`Restarts:`の下に、0から5までのオプションが表示されています。キーボードで数字を打つと、オプションを選択することができます。それぞれのオプションについて見ていきましょう。

`[ABORT]`を選択すると操作を中断して1段階前まで戻り、`[*ABORT]`ではトップレベルまで戻ります。`[CONTINUE]`ではコードの実行を続けます。`[USE-VALUE]`では特定の値を実行、`[STORE-VALUE]`では特定の値を保存することができます。

では、試しに`1: [USE-VALUE]`を選択します。ミニバッファに`Type a form to be evaluated:`と表示されるので、yの値に`4`を設定します。すると、REPLで評価結果が表示されます。

```
CL-USER> (f1 2) 
Type a form to be evaluated: 4 
 
6
```

#### ブレークポイント

次はフィボナッチ関数の定義です。再帰的に定義しています。
 
```lisp
(defun fib(n)
  (break)
  (if (<= 0 n 1)
      (/ 1 0)
      (+ (fib (- n 1))
         (fib (- n 2)))))
```

関数の最初に`(break)`を挿入すると、1段階ずつトレースすることができます。上記の定義では、整数をゼロで割る際にエラーが発生します。

`(break)`を挿入後、関数を再度コンパイルして、次のように関数を実行します。関数を実行すると、デバッガーが起動します。

```
(fib 9)
```

`Restarts`で`0: [Continue]`を選択しながらトレースしていきます。各スタックにカーソルを合わせてReturnを押すと、スタックの状態を確認することができます。例えば、`(FIB 5)`を選択することで、引数nの値が5であることが確認できます。

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
```

スタックが`(FIB 1)`まで来た時に、`DIVISION-BY-ZERO`エラーが起こるのが確認できます。

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
```

## キーバインド

デフォルトのキーバインドのうち、主なものは次の通りです。`M-x describe-bindings`で全てのキーバインドを確認することができます。

```
# カーソル移動
C-p: 1行上に移動　　　C-n: 1 行下に移動　C-f: 1文字前に移動　　C-b: 1 文字後に移動
C-a: 行の先頭に移動　 C-e: 行の末尾に移動

# 編集
C-_：最後の操作を取り消す
C-s：文字列を前方検索　　　　C-r：文字列を後方検索
C-d：カーソルの文字を削除　　C-h：カーソルの一文字前を削除
C-k：カーソルの位置から行末までカット
C-SPC：マークセット  C-w：切り取り　　M-w: コピー　　C-y：貼り付け
C-x Space: 矩形選択の開始

# ファイル操作系
C-x C-f：ファイルを開く　　C-x C-s：現在のバッファをディスクに保存
C-g: 現在の操作を中止

# ウィンドウ・バッファ関連
C-x 2：上下に分割　　C-x 3: 左右に分割　　C-x 1：分割したウィンドウを閉じる
M-o: 次のバッファに移動　　C-x C-b: バッファ一覧を表示
```

## vi-mode

### 初期設定

Lemにはviシュミレータのvi-modeが組み込まれています。vi-modeを有効にするには、`~/.lem/init.lisp`に次の1行を加えるか、エディタの起動後に`M-x vi-mode`とします。

 ```
(lem-vi-mode:vi-mode)
```

### モードの切り替え

vi-modeでは、viと同様、コマンドモードへは`esc`キー、インサートモードへは`i`、ビジュアルモードには`v`で切り替えます。カーソル移動やテキスト編集等、操作はviと同じです。


## 拡張機能の書き方

lemでの拡張機能の書き方を紹介します。プロジェクト名はposts-listとして、redditの投稿一覧のビューアを作ります。

完成は、次のようになります。

![完成図](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/screenshot.png)

上のスクリーンショットでは/r/lispの投稿のリストを取得して表示しています。選択するとリンク先をブラウザで開く機能などがついています。

### プロジェクトの作成

lemの拡張機能は、他のプロジェクトと同様に、ASDFのシステムとして扱います。ベースとするディレクトリの下に、拡張機能と同じ名前のディレクトリを用意し、その下に各ファイルを配置します。以下のコマンドでプロジェクトのテンプレートを生成します。

```
$ ros init lem posts-list
```

これでプロジェクトのベースが出来ました。ディレクトリツリーは以下のようになります。

```
$ tree posts-list
posts-list
├── lem-posts-list.asd
└── main.lisp
```

`M-x start-lisp-repl`でREPLを起動して、作成したプロジェクトを読み込みます。

```
CL-USER> (ql:quickload :lem-posts-list)
```

もしエラーが出て読み込めない場合、パスが通っていない可能性が高いです。デフォルトでは`$HOME/common-lisp`にパスが通ってあるので、posts-listディレクトリを`$HOME/common-lisp/`以下に配置してみてください。

### 記事のリストを取得

subredditを指定して、redditの記事をjsonで取得します。1つの記事をpostという構造体にして、postのリストを返す処理を用意します。これ自体はlemとは関係ないので`posts.lisp`に分離します。

```
(defpackage #:lem-posts-list/posts
  (:import-from #:jonathan)
  (:import-from #:dexador)
  (:import-from #:trivia
                #:match
                #:plist)
  (:import-from #:quri)
  (:use #:cl)
  (:export #:post-title
           #:post-url
           #:post-author
           #:fetch-posts))
(in-package #:lem-posts-list/posts)

(defstruct post title url author)

(defun extract-posts (data)
  (match data
    ((plist :|data| (plist :|children| children))
     (loop :for child :in children
           :collect (match child
                      ((plist :|data|
                              (plist :|title| title
                                     :|url| url
                                     :|author| author))
                       (make-post :title title
                                  :url url
                                  :author author)))))))

(defun make-posts-url (subreddit)
  (quri:make-uri :scheme "https"
                 :host "reddit.com"
                 :path (format nil "/r/~A/.json" subreddit)))

(defun fetch-posts (subreddit)
  (extract-posts (jojo:parse (dex:get (make-posts-url subreddit)))))
```

lem-posts-list/postsというパッケージに分離しました。fetch-postsという関数とpostのアクセサを外部から使いたいのでexportします。

fetch-postsはsubreddit名を引数で受け取りpostのリストを返します。REPLから動作を確認します。

```
CL-USER> (ql:quickload :lem-posts-list/posts)
CL-USER> (lem-posts-list/posts:fetch-posts "lisp")

;; 出力は長いので省略
```

### Lemへの表示

投稿一覧が取得できるようになったので、次はその内容をLemに表示し選択できるようにします。投稿一覧用のバッファを作り、そこにfetch-posts関数から返ってきたpostのリストを書き出していきます。その前にlemで扱うオブジェクトについていくつか見ていきます。

#### buffer

バッファはテキストとその色やカーソルの位置、モードなどが入ったオブジェクトです。通常はファイルを開くときにバッファはそのファイルと関連付けられますが、ファイルと関連付けずにバッファ自体を作成することも可能です。

バッファは`make-buffer`関数を使うことで作成できます。ファイルと関連付けられたバッファは`find-file-buffer`関数を使うことで作成できます。

```
(lem:make-buffer "test") ; => #<BUFFER test NIL>
(lem:find-file-buffer "/tmp/hoge") ; => #<BUFFER hoge /tmp/hoge>
```

#### point

ポイントはバッファ内の位置を指すオブジェクトです。主にカーソルなどに使われています。バッファ内への文字列の挿入や削除に使います。ポイントを扱う場合はバッファ内に既にあるポイントをコピーして使う事が多いです。バッファからポイントを得るアクセサは次のようなものがあります。

```
;; バッファの現在の位置のポイントを得る
(buffer-point buffer)

;; バッファの先頭の位置のポイントを得る
(buffer-start-point buffer)

;; バッファの末尾の位置のポイントを得る
(buffer-end-point buffer)
```

ポイントのコピーには`copy-point`関数を使います。

`(copy-point point &optional kind)`

ポイントはスティッキーな動作をします。そのポイントより前の位置に文字列を挿入するとその分右へずれていき、削除すると左にずれていきます。
 
`kind`にはバッファ編集時のオフセットを計算するときに使います。

`kind`が:left-insertingならポイントと同じ位置に文字列を挿入したときに右にずれ、:right-insertingならそのままです。

`kind`が:temporaryの場合は何も行いません。

`kind`を指定しばければ渡された`point`と同じ値になります。

`kind`が:temporary以外ならpointをbufferが保持しておく必要があるので不要になったら明示的に削除しなければいけません。
削除には`delete-point`関数を使います。

```
(let ((point (copy-point (buffer-point buffer) kind)))
  (unwind-protect ...
    (delete-point point)))
```

このために`with-point`マクロを用意しています。

```
(with-point ((point (buffer-point buffer) kind))
  ...)
```

`with-point`の`kind`を省略した場合は:temporaryになります。


## まとめ

* LemはCommon Lisperのエディタ。特別な設定なしにCommon Lispで開発を始めることができる。

* RoswellでLemをインストールすると`lem <ファイル名>`でLemを起動することができるようになる。

* LemのREPLでシステムを読み込むと、`M-.`でシンボルの定義にジャンプ、`M-,`で元の場所に戻ることができる。

* `C-c C-m`で1段階のマクロ展開(macroexpand-1)、`C-c M-m`で全段階のマクロ展開をすることができる。

* LemにはSLIMEが組み込まれている。SLIMEはCommon Lispの統合開発環境であり、効率的にデバッグやインスペクトができる。

* SLIMEのインスペクタを用いると、シンボルの内部構造を調べたり、シンボル内部の値を変更することができる。

* SLIMEにはSLDB(SLIME debugger)というデバッガが搭載されている。

* コード内でブレークポイントを設定するには`(break)`を挿入する。

* キーバインドはEmacs同等のものかvi-modeを選択できる。

* 拡張機能を書くには、拡張機能のためのテンプレートを`ros init lem <拡張機能の名前>`で生成できる。
