# Lem

Lemは、Common Lispでの開発に必要な機能が一通り組み込まれているエディタです。入力補完機能や、Common Lisp処理系とのやりとりを行う機能(SLIME)などがデフォルトで組み込まれているため、インストール後すぐに開発を始めることができます。また、Lem自体もCommon Lispで実装されており、Common Lispを使って拡張機能を作ることができます。Lemは初回起動時にコンパイル済みの実行イメージをつくるため、以降の起動が非常に速いという特徴があります。
 
本章では、Lemのインストール、基本的な操作法、SLIMEの使い方、vi-mode、拡張機能の書き方を紹介します。基本的な操作法はEmacsと共通しているので、すでにEmacsでSLIMEの利用経験がある方は、拡張機能の節から読み始めてもいいでしょう。
 
## Lemのインストール

LemをインストールするにはRoswellを使うのが簡単です。次のコマンドでインストールできます。

```
$ ros install cxxxr/lem
```

このようにインストールすると、Lemの実行ファイルが`~/.roswell/bin/`以下にできます。`.bashrc`等の設定ファイルで次のように`PATH`環境変数を設定しておくと、以降は`lem`コマンドで起動できるようになります。


```
export PATH=$PATH:~/.roswell/bin
```

### Lemの更新
Lemを最新の状態にするには、`ros update lem`を実行します。

## コマンド表記と入力方法について

本章では、`C-x`や`M-x`等のコマンド表記が出てきますが、それぞれのコマンド表記に対応する実際の入力方法は次の表のようになります。

| コマンド表記 | 実際の入力方法 |
|:-----------:|:------------:|
| `C-c` | `Control`キーを押しながら`C`を押す |
| `M-x` | `Meta`キー(`Esc`か`Alt`キーであることが多い)を押しながら`X`を押す |
| `C-x C-e` | `Control`キーを押しながら`X`を押し、そのまま`Control`キーを押したまま`E`を押す |
| `C-x o` | `Control`キーを押しながら`X`を押し、`Control`キーを離してから`O`を押す |

## Lemの起動と終了

lemの起動は`lem`コマンドを使います。
引数にファイル名を指定するとそのファイルを開いて起動します。
また、ディレクトリ名を指定するとディレクトリを開いて起動します。

```
# ファイル名を指定して開く
$ lem <ファイル名>

# ディレクトリを開く
$ lem <ディレクトリ名>
```

```
/home/user/.roswell/

      7k 2019/02/15 13:59:19 Fri ..
     352 2019/02/11 21:45:49 Mon archives/
     288 2019/02/15 13:59:17 Fri bin/
      96 2018/12/02 15:28:08 Sun env/
     128 2018/12/02 15:27:29 Sun impls/
      96 2018/12/02 15:38:29 Sun lib/
```

ファイルの保存は`C-x C-s`で行い、`C-x C-c`でLemを終了することができます。ファイルを保存せずに終了しようとすると確認を求められます。

## 基本的な使い方
以下の表は主に使うコマンド一覧です。

### カーソル移動
|キー                                  |説明                                  |
|--------------------------------------|--------------------------------------|
|C-p,C-n,C-b,C-f または カーソルキー   |カーソルを上下左右に移動              |
|C-a,C-e または HOME, END              |カーソルを行頭、行末に移動            |
|M-b,M-f                               |カーソルを単語単位で左右に移動        |
|C-v,M-v または PageUp,PageDown        |カーソルを一画面分上下に移動          |
|M-<,M->                               |バッファの先頭、末尾にカーソルを移動  |
|C-M-p,C-M-n,C-M-b,C-M-f               |カーソルを式単位で上下左右に移動      |
|M-{, M-}                              |次の空行までカーソルを移動            |

### 編集操作
|キー                                  |説明                                                       |
|--------------------------------------|-----------------------------------------------------------|
|C-d または Delete                     |カーソル位置の右を一文字削除                               |
|C-h または Backspace                  |カーソル位置の左を一文字削除                               |
|C-k                                   |カーソル位置から行末までを削除                             |
|C-M-h または M-Backspace              |単語単位で前方の文字列を削除                               |
|M-d                                   |単語単位で後方の文字列を削除                               |
|C-o                                   |カーソル位置に改行を挿入                                   |
|C-x C-o                               |カーソル位置の複数の空白行を削除                           |
|C-Space, C-@                          |カーソル位置をマーク                                       |
|C-w                                   |マークした位置からカーソル位置までを削除                   |
|M-w                                   |マークした位置からカーソル位置までをコピー                 |
|C-M-k                                 |式単位で後方の文字列を削除                                 |
|C-M-t                                 |前方の式と後方の式を交換                                   |
|C-y                                   |コピー範囲や一度に複数の文字を削除したテキストを貼り付け   |
|C-\, M-_                              |Undo, Redo                                                 |

### ファイル、バッファ操作
|キー                                  |説明                                  |
|--------------------------------------|--------------------------------------|
|C-x C-f                               |ファイルを開く                        |
|C-x C-r                               |ファイルを読み込み専用で開く          |
|C-x C-s                               |ファイルを保存                        |
|C-x C-w                               |名前を付けてファイルを保存            |
|C-x s                                 |開いているファイルを全て保存          |
|C-x b                                 |バッファ切り替え、存在しなければ作成  |
|C-x k                                 |バッファを削除                        |

### ウィンドウ操作
|キー                                  |説明                                  |
|--------------------------------------|--------------------------------------|
|C-x 2                                 |ウィンドウを上下に分割                |
|C-x 3                                 |ウィンドウを左右に分割                |
|C-x 1                                 |ウィンドウを一つにする                |
|C-x 0                                 |現在のウィンドウを閉じる              |
|C-x o, M-o                            |ウィンドウを移動                      |

### 検索と置換
|キー            |説明|
|----------------|------------------------------------------------------------|
|C-s             |次を検索                                                    |
|C-r             |前を検索                                                    |
|C-M-s           |正規表現で次を検索                                          |
|C-M-r           |正規表現で前を検索                                          |
|M-s _           |シンボル単位で次を検索                                      |
|M-s M-_         |シンボル単位で前を検索                                      |
|M-s .           |カーソル位置にあるシンボル名で検索                          |
|検索中にEnter   |検索にマッチした箇所のハイライトを保つ                      |
|F2              |検索でマッチしてハイライトしている箇所を置換                |
|M-%             |対話置換                                                    |

コマンド一覧は`M-x describe-bindings`で表示されます。
このときに出るポップアップウィンドウはMoreとよく似た操作方法でSpaceで1ページ分スクロール、閉じるには最後のページまでスクロールされた状態でSpaceを押すか、`q`を入力します。

デフォルトではemacsの操作にある程度合わせています。
viに合わせたい場合は`M-x vi-mode`と入力することで切り替えられます。
元に戻すには`M-x emacs-mode`です。
起動時に自動でvi-modeにするには`~/.lem.d/init.lisp`に次の一行を加えます。
```
(lem-vi-mode:vi-mode)
```

## SLIME
SLIMEは`Superior Lisp Interaction Mode for Emacs`の略であり、Emacs上でCommon Lispでの開発を行うためのEmacs Lispプラグインの名前です。Common Lisp処理系でSWANKと呼ばれるサーバを起動し、SLIMEはエディタ側でSWANKサーバと通信することで、式の評価やコンパイル、シンボルの補完や正確なインデント、デバッガやインスペクタなど、広範の機能を提供します。
SLIMEはEmacsだけでなくvimやatom上での実装もあり、Lemでも同様の機能がCommon Lispで実装されています。
SLIMEを使うことで、SWANKサーバを介してCommon Lisp処理系と対話しながらLispアプリケーションを構築していくことができます。

### lisp-mode
LemではCommon Lispの開発に便利な機能をlisp-modeとして提供し、内部でSLIMEを使っています。
lispファイルを開くか、明示的に`M-x lisp-mode`とするとlisp-modeが有効化されます。

### SWANKサーバへの接続
lisp-modeが有効化されたときにSWANKサーバに接続していない場合は、自動でLemランタイム上でサーバを起動し、接続します。
LemはCommon Lispで書いているのでLemとSWANKサーバを同じランタイム上で動かせ、それをすることでLem自身の状態の変更をSLIMEの機能を介して行うことが出来ます。

REPLを開くには`start-lisp-repl`コマンドを使います。

```
M-x start-lisp-repl
```

lemとは別のプロセスを起動してSLIMEで接続するにはslimeコマンドを使います。
このコマンドはREPLも同時に開きます。

```
M-x slime
```

`C-u`を前に入力してslimeコマンドを使うと起動する処理系を選べます。
```
C-u M-x slime
```

### REPL

試しにREPLに式を打ち込んでみます。

```
CL-USER> (cons 'a '(b c))
(A B C)
CL-USER> (mapcar #'1+ '(1 2 3)) 
(2 3 4)
```

`M-p` `M-n`でREPLの履歴を辿れます。

実行を中断したい場合はC-c C-cで出来ます。

```
CL-USER> (loop)
;; C-c C-cで中断 デバッガがqを入力してreplに戻る
CL-USER>
```

中断すると割り込みエラーでデバッガが表示されます。
デバッガの使い方については後述します

### 入力補完

Lemでは入力中にTabを押すことで補完が出来ます。
例えば`defst`と打ち込んだ後にTabキーを押すと、次のように候補が表示されます。

![補完](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-completion.png)

SWANKサーバのランタイム上に存在するシンボルから補完候補を出すので、補完したいライブラリのシンボルは、そのライブラリを読み込むことで出来ます。

ここではcl-ppcreのシンボルを補完をしたいのでREPLでcl-ppcreをquickloadしてみます。
```
CL-USER> (ql:quickload :cl-ppcre) 
To load "cl-ppcre":
  Load 1 ASDF system:
    cl-ppcre
; Loading "cl-ppcre"
.
(:CL-PPCRE)
```

exportされたシンボルはコロンを一つ付けることで補完候補に出ます。
![](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-completion-1.png)

exportされていないシンボルは、2つコロンをつけると補完候補に出ます。
![](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-completion-2.png)

補完は曖昧検索を使っているので並びさえあっていれば補完候補に出ます。
![](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-completion-3.png)

### 式の評価とコンパイル

lispファイル上の式を評価してみます。
例としてfoo.lispというファイルを開きます。

```lisp
;;; foo.lisp

(defun foo (x)
  (1+ x))
```

関数fooの中で`C-M-x`とすると、そのdefunが評価され、定義されます。

REPLで定義した関数を呼びだしてみます。
```lisp
CL-USER> (foo 0)
1
```

`C-M-x`の代わりに`C-c C-c`を使うと評価ではなくコンパイルされロードされます。
コンパイルするとコンパイル時の警告部分が赤線で引かれます。
![](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-compile.png)

コンパイル時に出てきたウィンドウにの指定箇所にカーソルを合わせてEnterを押すことで特定箇所にジャンプすることが出来ます。
`C-x C-n` `C-x C-p`を使って順番に特定箇所にジャンプすることも可能です。

赤線を消すには警告箇所を修正して再度`C-c C-c`します。
`C-c M-c`をしてもバッファ内の全ての赤線を消すことが出来ます。

ファイル自体を読み込むには`C-c C-l`をします。
コンパイルし、その結果を読み込むには`C-c C-k`をします。

カーソルの前の式を評価するには`C-c C-e`をします。
```lisp
(progn
  (foo)
  (bar) ; <- (bar)だけ評価するにはここでC-c C-e
  (baz))
```

ミニバッファに式を入力し、評価するには`C-c M-:`とします。

評価中の式が無限ループをして終わらない場合などはREPLでの中断とは別に`C-c g`をして中断ができます。

評価は基本的に現在接続しているSWANKサーバで行いますが、その接続しているプロセスとは別にLemのカスタマイズをしたいなどの理由でLemのプロセス内で評価するコマンドも用意しています。
カーソルの前の式をLemプロセス内で評価する場合は`C-c C-e`の代わりに`C-x C-e`、
ミニバッファで入力した式をLemプロセス内で評価する場合は`C-c M-:`の代わりに`M-:`を使います。

### 定義へのジャンプ
関数や変数、クラスなどの定義位置を参照する機能があります。
`M-. (M-x find-definitions)`を使うことでソースコードの適当箇所にジャンプすることができます。
カーソル位置にシンボルがある場合はその定義位置へジャンプし、無ければミニバッファからシンボル名を入力します。
元の位置に戻るには`M-, (M-x pop-definition-stack)`を使います。

この機能はSWANKサーバ側でシンボルを参照するので、事前にそのシンボルが定義されているようにシステムを読み込んでおかなければなりません。

```
CL-USER> (ql:quickload システム名)
```

適当する定義が複数ある場合は定義箇所にジャンプする前に一覧が別ウィンドウに表示されます。
次の画像はcl-ppcre:scanを対象にした例です。
![](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-jump-to-definitions.png)

この場合は`C-x C-n` `C-x C-p`で定義箇所に順番にジャンプできます。
一覧が表示されたウィンドウに移動して見たい定義にカーソルを合わせてEnterを押すことでも定義位置にジャンプできます。
この操作方法はコンパイラの警告の一覧と同じす。この機能は他にもgrepなどで使われています。

### Apropos
Common Lispで存在するシンボルを検索するにはaproposという関数を使います。

```lisp
CL-USER> (apropos "MULTIPLE-VALUE")
SB-C::IR1-CONVERT-MULTIPLE-VALUE-CALL (fbound)
SB-C::IR1-CONVERT-MULTIPLE-VALUE-PROG1 (fbound)
SB-EVAL::EVAL-MULTIPLE-VALUE-CALL (fbound)
SB-EVAL::EVAL-MULTIPLE-VALUE-PROG1 (fbound)
SB-WALKER::WALK-MULTIPLE-VALUE-BIND (fbound)
SB-WALKER::WALK-MULTIPLE-VALUE-SETQ (fbound)
SWANK::MULTIPLE-VALUE-OR (fbound)
MULTIPLE-VALUE-BIND (fbound)
MULTIPLE-VALUE-CALL (fbound)
MULTIPLE-VALUE-LIST (fbound)
MULTIPLE-VALUE-PROG1 (fbound)
MULTIPLE-VALUE-SETQ (fbound)
MULTIPLE-VALUES-LIMIT (bound)
```

SLIMEではエディタからaproposを使えるインターフェースを用意しています。

* `C-c C-d a (M-x lisp-apropos)`でexportされたシンボルの大文字小文字を区別しない検索
* `C-u C-c C-d a (C-u M-x lisp-apropos)`でミニバッファからexportされているか、大文字小文字を区別するかを選んで検索
* `C-c C-d z (M-x lisp-apropos-all)`で全てのパッケージの全てのシンボルを大文字小文字区別せずに検索
* `C-c C-d p (M-x lisp-apropos-package)`で指定したパッケージの全てのシンボルを表示

があります。

次の画像はcl-ppcreのscanを検索した例です。
![](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-apropos.png)

この検索結果のシンボルの位置でReturnを押すと定義箇所へのジャンプが出来ます。

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

## 拡張機能の書き方

lemでの拡張機能の書き方を紹介します。プロジェクト名はposts-listとして、redditの投稿一覧のビューアを作ります。

完成は、次のようになります。

![完成図](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-extension-preview.png)

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
