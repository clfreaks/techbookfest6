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
このときに出るポップアップウィンドウはTypeoutウィンドウと呼ばれるものです。
Moreとよく似た操作方法でSpaceで1ページ分スクロール、閉じるには最後のページまでスクロールされた状態でSpaceを押すか、`q`を入力します。

デフォルトではemacsの操作にある程度合わせています。
viに合わせたい場合は`M-x vi-mode`と入力することで切り替えられます。
元に戻すには`M-x emacs-mode`です。
起動時に自動でvi-modeにするには`~/.lem.d/init.lisp`に次の式を加えます。
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

### インデント
カーソルの行を字下げするにはTabを入力します。
一つの式をまとめてインデントするには`C-M-q`です。

```lisp
(defun fact (n)
(if (= n 0)
1
(* n (fact (1- n)))))
```

ここで`(defun`の先頭にカーソルを合わせて`C-M-q`とすると次のようにインデントされます。

```lisp
(defun fact (n)
  (if (= n 0)
      1
      (* n (fact (1- n)))))
```

あとから追加されたマクロなどで特別なインデントをしたい場合は、そのマクロをSWANKサーバ側で定義さていると出来ます。
例えばcl-ppcreにregister-groups-bindというマクロがありますが、これはcl-ppcreを読み込んでない状態では関数と同じインデントにされてしまいます。
cl-ppcreを読み込むと正しくインデントできます。

```
CL-USER> (ql:quickload :cl-ppcre)
```

実際に試してみると次のようにインデントされます。

```
(ppcre:register-groups-bind (key value)
    ("(\\w+):(\\w+)" "foo:bar")
  (cons key value))
```

静的にインデントを設定したい場合は`lem-lisp-syntax:set-indentation`を使います。
`~/.lem.d/init.lisp`に次の式を追加してみます。

```
(lem-lisp-syntax:set-indentation
 "with-mock-functions"
 (lem-lisp-syntax.indent:get-indentation "flet"))
```

これで起動時にwith-mock-functionsというフォームはfletと同じ形にインデントされるようになります。

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

### マクロ展開
Common Lispではマクロ展開をするための関数としてmacroexpandがあり、一段階だけ展開したい場合の関数としてmacroexpand-1があります。

lemからマクロ展開をするには`C-c C-m (M-x lisp-macroexpand)`を使います。
このコマンドは一段階だけマクロを展開します。

マクロ展開にはSWANKサーバ側でそのマクロを定義されている必要があるため、事前にマクロを評価しておく必要があります。

例として[clhs](http://clhs.lisp.se/Body/f_mexp_.htm) のExamplesで定義されているマクロ`alpha`, `beta`でマクロ展開してみます。

```lisp
(defmacro alpha (x y) `(beta ,x ,y))
(defmacro beta (x y) `(gamma ,x ,y))
```

`(alpha 1 2)`というマクロを一段階だけ展開したい場合は最初の括弧の位置にカーソルを置いて`C-c C-m`します。
`(alpha (beta 1 2) 3)`という式を全て展開したい場合は`(alpha`の先頭の括弧にカーソルを置いて`C-c M-m`します。

試してみると展開結果がTypeoutウィンドウに出力されます。
この展開結果で更にマクロ展開したい場合は同じようにカーソルを展開したいマクロの開き括弧に合わせて`C-c C-m`とします。

### 定義へのジャンプ
ある関数や変数、クラスなどの定義位置を参照する機能があります。
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

### シンボルが使われている場所の一覧
ある関数や変数、クラスなどがどこで使われているかを参照したい場合があります。
`M-_ (M-x find-references)`を使うことでそのシンボルの関数がどこから呼ばれているか、変数がどこから参照されているかを確認できます。
これは定義へのジャンプと使い方が同じで、事前にシステムの読み込みも同じく必要です。
違う点は定義側とそれを使っている側という点です。

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
