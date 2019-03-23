# Lem
LemはCommon Lispが動作するランタイム上のエディタです。

Common Lispでの拡張を想定しており、Common Lispの開発環境を主に提供しています。  
それ以外にもLemの開発者が主に使う言語を中心にサポートしています。

操作体系は作者がemacsを使っていたこともあり、emacsによく似ていますが
他のエディタにある機能も取り込むようにしており、vi(vim)のモードが用意されています。

また、メジャーなCommon Lisp処理系にあるイメージダンプ機能の利点を活かして、ライブラリの読み込み、メモリへのデータの配置状態を実行ファイルの形にしていて、高速に起動できる特徴もあります。
 
## Lemのインストール

LemをインストールするにはRoswellを使うのが簡単です。  
次のコマンドでインストールできます。

```
$ ros install cxxxr/lem
```

パスの設定は次のとおりです。

```
export PATH=$PATH:~/.roswell/bin
```

次のコマンドでlemを最新の状態に更新できます。

```
$ ros update lem
```

## Lemで使う用語

LemではEmacsと同じようにControlやMetaをプリフィクスとするコマンドを使います。

*C-* コントロールキーを押しっぱなしにして別のキーを打つことを意味します。  

*M-* メタキーを押しっぱなしにして別のキーを打つことを意味します。メタキーはAltキーを使い、Macのターミナルでは設定でOptionキーに割り当てられます。

たとえば  
`C-x o`はControlを押しながらxを押したあと、Controlを離してoを押します。  
`C-x C-o`だとControlを押しながらxを押し、更にControlを押しながらoを押します。

### ミニバッファ
画面の一番下で入力が出来る空間のことです。

### バッファ
バッファはファイルと関連付けられたデータ構造を表します。

### ウィンドウ
ウィンドウは一つのバッファを画面に表示するデータ構造を表します。

## Lemの起動と終了
コマンドラインからLemを起動するにはlemコマンドを使います。
lemコマンドはRoswellからインストールしている場合に使えます。

```
$ lem [ファイル名]
```

コマンドラインで起動したCommon LispのREPLから使うには次のとおりです。

```
* (ql:quickload :lem-ncurses)
* (lem:lem) ;; sbclの場合は(ed)でも可能
```

起動時の画面は次のようになります。
![](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-startup.png)

Lemでは起動時にREPLが始まります。
REPLについては後述します。

終了するには`C-x C-c`と入力してください。

## 初期化ファイルについて

lemを起動時に初期化ファイルが読み込まれます。
ファイル名は`~/.lemrc`または`~/.lem.d/init.lisp`です。

以下のように書きます。

```lisp
:: -*- mode:lisp -*-

(in-package :lem-user)

;; ここから設定を記述
```

一例として次のリポジトリが参考になります。  
https://github.com/fukamachi/.lem/

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
SLIME(Superior Lisp Interaction Mode for Emacs)は元はEmacs上でCommon Lispの開発を行えるようにするEmacs用のプロダクトでした。
Common Lispランタイム上で動作するswnakサーバがあり、Emacs上のslimeとクライアントサーバ方式でswank rpcという独自プロトコルでやりとりを行い、パワルルなREPL、補完、デバッガ、インスペクタなどを提供します。
今もよく使われていますが、vimやatomなどのエディタでもslimeの実装が開発されていて、lemも同様にslimeを実装し、デフォルトでサポートしています。

### lisp-mode
LemではCommon Lispの開発に便利な機能をlisp-modeとして提供し、内部でSLIMEを使っています。
lispファイルを開くか、明示的に`M-x lisp-mode`とするかREPLが開かれるとlisp-modeが有効化されます。

### SWANKサーバへの接続
lisp-modeが有効化されたときにSWANKサーバに接続していない場合は、自動でLemランタイム上でサーバを起動し、接続します。
LemはCommon Lispで書いているのでLemとSWANKサーバを同じランタイム上で動かせ、それをすることでLem自身の状態の変更をSLIMEの機能を介して行うことが出来ます。

起動時にはREPLが開かれていますが、明示的にREPLを開くには`start-lisp-repl`コマンドを使います。

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

ためしに何か入力してみます。

```
CL-USER> (loop :for i :from 0 :below 100 :sum i)
5050
```

REPLは基本的にコマンドラインからの使用と同じですがSLIMEで追加された機能があります。

履歴を辿るには`M-p` `M-n`で行えます。

現在のパッケージを切り替えるには`(in-package パッケージ名)`を入力することでも出来ますが`C-c M-p (M-x lisp-set-package)`でもREPLのパッケージを切り替えられます。

`C-c M-o (M-x listener-clear-buffer)`でバッファをクリアできます。

`C-c C-u (M-x listener-clear-input)`で現在の入力を消去します。

実行を中断したい場合は`C-c C-c (M-x lisp-repl-interrupt)`です。

```
CL-USER> (loop)
;; C-c C-cで中断 デバッガでqを押してreplに戻る
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

![](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-completion.png)

このときに`C-n`や`M-n`、カーソルキーの下を入力すれば一つ下の候補を選べます。  
上にするには`C-p` `M-p` カーソルキーの上を入力します。  
Enterを押すことでその補完候補を選択できます。

lisp-modeではあいまい補完を使っているので並びがあっていれば補完候補に表示されます。  
補完機能はミニバッファでの入力などでも出来るので、例えばファイルを開く(`C-x C-f`)場合はファイル名が補完されます。

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

### インスペクタ
Common Lispではオブジェクトの中身を見たり、必要ならその場で値を変更できるinspectという関数があります。
SLIMEではエディタから使うためのインターフェースを提供しています。

試しにlemのウィンドウをinspectしてみます。
RPELで取り出したい式を評価し、その評価結果の値をinspectしてみます。

```
CL-USER> (lem:current-window)
#<LEM::WINDOW {10052AEC83}>
```

`C-c I (M-x lisp-inspect)`でinspectを始められます。

```
Inspect Value (evaluated): *
```

Common Lispでは最後にREPLで評価した値が`*`に入るので、`(lem:current-window)`の評価結果がinspectされます。

![](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-inspect-1.png)

`Tab`で次の選択できる位置までカーソルが移動され`Enter`で選択でき、値を選択するとその値をさらにinspectできます。  
左側の`[ ]`を選択するとチェックがつき。その状態で[set value]を選択するとその値を変更することが出来ます。

inspectの操作の一覧は次のとおりです。

|キーバインド                                    |説明                                        |
|------------------------------------------------|--------------------------------------------|
|l (M-x lisp-inspector-pop)                      |一つ前のinspectに戻る                       |
|n (M-x lisp-inspector-pop) またはSpace          |次のinspectに進む                           |
|d (M-x lisp-inspector-describe)                 |inspectしているオブジェクトの詳細を表示     |
|p (M-x lisp-inspector-pprint)                   |カーソル位置のオブジェクトをpprint          |
|e (M-x lisp-inspector-eval)                     |inspect画面でeval                           |
|h (M-x lisp-inspector-history)                  |履歴を表示                                  |
|g (M-x lisp-inspector-reinspect)                |inspect画面の更新                           |
|. (M-x lisp-inspector-show-source)              |カーソル位置のオブジェクトの定義にジャンプ  |
|q (M-x lisp-inspector-quit)                     |inspectを終了                               |

試しにlemのウィンドウの中のmodeline-formatを変更してみます。  
modeline-foramtを選択しチェックを付け、`[set value]`を選択するとミニバッファに次の入力画面が出ます。

```lisp
Set slot LEM:MODELINE-FORMAT to (evaluated) :
```

たとえば次のように入力するとREPLウィンドウのモードラインの見た目が変更されます。

```lisp
Set slot LEM:MODELINE-FORMAT to (evaluated) : 
`(("-----------------HELLO WORLD------------------"
  ,(lem:make-attribute :foreground "red" :background "white" :bold-p t)))
```

![](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-inspect-2.png)

今動かしているエディタ自体をinspectで動的に変更することはエディタを簡単に壊せてしまいますが、とても魅力的な機能です。

他の使いどころとしてはhttp-requestで返ってきたレスポンスの中身を覗く場合に使います。  
クラスのインスタンスやハッシュテーブルの中身はREPLの結果からは見えないので、inspectで表示し、さらにその中身をinspectすることで
オブジェクトブラウザとして使うことが出来ます。

### SLDB(デバッガ)
SLIMEでのデバッガはSLDBと呼ばれています。
評価した式でエラーが起こった場合はSLDBが起動し、専用のsldbバッファが表示されます。

次の画像はREPLでエラーが出る式を評価してSLDBが出た例です。
![](https://raw.githubusercontent.com/clfreaks/techbookfest6/master/images/02-lem-sldb-1.png)

alexandria:lastcarは引数にリストを期待していますが文字列を渡しているので型エラーが出ています。

色々書いてますが一つずつ内容を見ていきます。

まず一番上の

```
The value
  "foo"
is not of type
  LIST
   [Condition of type TYPE-ERROR]
```
ですが、これはエラーメッセージとコンディションの型です。

次に

```
Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1005708413}>)
```
これはエラー後にどういう対応をするかをユーザーに尋ねています。
Common Lispにはリスタートという機構があり、プログラムの例外時などにユーザーに次に何をするかの判断を委ねることが出来ます。
この場合は0と1と2という選択があります。
SLDBではそのバッファで数字を押すかカーソル位置を色が変えられて表示されている[...]に合わせてEnterを押すと選択できます。

最後に

```
Backtrace:
  0: (ALEXANDRIA.0.DEV:LASTCAR "foo")
  1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (ALEXANDRIA.0.DEV:LASTCAR "foo") #<NULL-LEXENV>)
  2: (EVAL (ALEXANDRIA.0.DEV:LASTCAR "foo"))
 --more--
```
がありますが、これは他の言語にもよくあるバックトラックを表示しています。
--more--と表示されている位置でEnterを押すと更に前の省略されているフレームも表示されます。
フレームの位置にカーソルを移動してEnterを押すとローカル変数などの詳細が表示されます。

`Tab`で次の選択できる要素に移動します。
`n`, `p`でカーソルを上下のフレームに移動できます。
`M-n`, `M-p`で上下のフレームに移動し、そのフレームの詳細を表示できます。

デバッガは終了するには`q`で出来ます。
