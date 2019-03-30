
= Lem


== Lemのインストール


LemをインストールするにはRoswellを使うのが簡単です。@<br>{}
次のコマンドでインストールできます。


//emlist{
$ ros install cxxxr/lem
//}


lemは256色をサポートしたターミナルで使うことを想定しています。
linuxのターミナルの場合、デフォルトでは8色しか表示できない事が多いのでTERM変数を変更してください。


//emlist{
$ export TERM=xterm-256color
//}

== Lemで使う用語


LemではEmacsと同じようにControlやMetaをプリフィクスとするコマンドを使います。



@<b>{C-} コントロールキーを押しっぱなしにして別のキーを打つことを意味します。  



@<b>{M-} メタキーを押しっぱなしにして別のキーを打つことを意味します。メタキーはAltキーを使い、Macのターミナルでは設定でOptionキーに割り当てられます。



たとえば@<br>{}
@<tt>{C-x o}はControlを押しながらxを押したあと、Controlを離してoを押します。@<br>{}
@<tt>{C-x C-o}だとControlを押しながらxを押し、更にControlを押しながらoを押します。


=== ミニバッファ


画面の一番下で入力が出来る空間のことです。


=== バッファ


バッファはファイルと関連付けられたデータ構造を表します。


=== ウィンドウ


ウィンドウは一つのバッファを画面に表示するデータ構造を表します。


=== コマンド


コマンドはエディタを操作する機能です。
 カーソルを動かしたり文字を挿入する処理は一つのコマンドとして定義されます。
コマンドはキーに割り当てられたり@<tt>{M-x}の後、ミニバッファでコマンド名を入力することでも実行できます。
以後@<tt>{(M-x コマンド名)} と表記します。


== 初期化ファイルについて


lemを起動時に初期化ファイルが読み込まれます。
ファイル名は@<tt>{~/.lemrc}または@<tt>{~/.lem.d/init.lisp}です。



以下のように書きます。


//emlist[][lisp]{
:: -*- mode:lisp -*-

(in-package :lem-user)

;; ここから設定を記述
//}


一例として次のリポジトリが参考になります。@<br>{}
https://github.com/fukamachi/.lem/


== コマンド一覧


コマンド一覧は@<tt>{M-x describe-bindings}で表示されます。
このときに出るポップアップウィンドウはTypeoutウィンドウと呼ばれるものです。
Moreとよく似た操作方法でSpaceで1ページ分スクロール、閉じるには最後のページまでスクロールされた状態でSpaceを押すか、@<tt>{q}を入力します。



== vi-mode


デフォルトではemacsの操作にある程度合わせています。
viに合わせたい場合は@<tt>{M-x vi-mode}と入力することで切り替えられます。
元に戻すには@<tt>{M-x emacs-mode}です。
起動時に自動でvi-modeにするには@<tt>{~/.lem.d/init.lisp}に次の式を加えます。


//emlist{
(lem-vi-mode:vi-mode)
//}

== SLIME


SLIME(Superior Lisp Interaction Mode for Emacs)は元はEmacs上でCommon Lispの開発を行えるようにするEmacs用のプロダクトでした。
Common Lispランタイム上で動作するswnakサーバがあり、Emacs上のslimeとクライアントサーバ方式でswank rpcという独自プロトコルでやりとりを行い、パワルルなREPL、補完、デバッガ、インスペクタなどを提供します。
今もよく使われていますが、vimやatomなどのエディタでもslimeの実装が開発されていて、lemも同様にslimeを実装し、デフォルトでサポートしています。


=== lisp-mode


LemではCommon Lispの開発に便利な機能をlisp-modeとして提供し、内部でSLIMEを使っています。
lispファイルを開くか、明示的に@<tt>{M-x lisp-mode}とするかREPLが開かれるとlisp-modeが有効化されます。


=== SWANKサーバへの接続


lisp-modeが有効化されたときにSWANKサーバに接続していない場合は、自動でLemランタイム上でサーバを起動し、接続します。
LemはCommon Lispで書いているのでLemとSWANKサーバを同じランタイム上で動かせ、それをすることでLem自身の状態の変更をSLIMEの機能を介して行うことが出来ます。



REPLを開くには@<tt>{start-lisp-repl}コマンドを使います。


//emlist{
M-x start-lisp-repl
//}


lemとは別のプロセスを起動してSLIMEで接続するにはslimeコマンドを使います。
このコマンドはREPLも同時に開きます。


//emlist{
M-x slime
//}


@<tt>{C-u}を前に入力してslimeコマンドを使うと起動する処理系を選べます。


//emlist{
C-u M-x slime
//}

=== REPL


ためしに何か入力してみます。


//emlist{
CL-USER> (loop :for i :from 0 :below 100 :sum i)
5050
//}


REPLは基本的にコマンドラインからの使用と同じですがSLIMEで追加された機能があります。



履歴を辿るには@<tt>{M-p} @<tt>{M-n}で行えます。



現在のパッケージを切り替えるには@<tt>{(in-package パッケージ名)}を入力することでも出来ますが@<tt>{C-c M-p (M-x lisp-set-package)}でもREPLのパッケージを切り替えられます。



@<tt>{C-c M-o (M-x listener-clear-buffer)}でバッファをクリアできます。



@<tt>{C-c C-u (M-x listener-clear-input)}で現在の入力を消去します。



実行を中断したい場合は@<tt>{C-c C-c (M-x lisp-repl-interrupt)}です。


//emlist{
CL-USER> (loop)
;; C-c C-cで中断 デバッガでqを押してreplに戻る
CL-USER>
//}


中断すると割り込みエラーでデバッガが表示されます。
デバッガの使い方については後述します


=== インデント


カーソルの行を字下げするにはTabを入力します。
一つの式をまとめてインデントするには@<tt>{C-M-q}です。


//emlist[][lisp]{
(defun fact (n)
(if (= n 0)
1
(* n (fact (1- n)))))
//}


ここで@<tt>{(defun}の先頭にカーソルを合わせて@<tt>{C-M-q}とすると次のようにインデントされます。


//emlist[][lisp]{
(defun fact (n)
  (if (= n 0)
      1
      (* n (fact (1- n)))))
//}


あとから追加されたマクロなどで特別なインデントをしたい場合は、そのマクロをSWANKサーバ側で定義さていると出来ます。
例えばcl-ppcreにregister-groups-bindというマクロがありますが、これはcl-ppcreを読み込んでない状態では関数と同じインデントにされてしまいます。


//emlist[][lisp]{
(ppcre:register-groups-bind (key value)
                            ("(\\w+):(\\w+)" "foo:bar")
                            (cons key value))
//}


cl-ppcreを読み込むと正しくインデントできます。


//emlist{
CL-USER> (ql:quickload :cl-ppcre)
//}

//emlist[][lisp]{
(ppcre:register-groups-bind (key value)
    ("(\\w+):(\\w+)" "foo:bar")
  (cons key value))
//}


静的にインデントを設定したい場合は@<tt>{lem-lisp-syntax:set-indentation}を使います。
@<tt>{~/.lem.d/init.lisp}に次の式を追加してみます。


//emlist[][lisp]{
(lem-lisp-syntax:set-indentation
 "with-mock-functions"
 (lem-lisp-syntax.indent:get-indentation "flet"))
//}


これで起動時にwith-mock-functionsというフォームはfletと同じ形にインデントされるようになります。


=== 入力補完


Lemでは入力中にTabを押すことで補完が出来ます。



//image[02-lem-completion][]{
//}




このときに@<tt>{C-n}や@<tt>{M-n}、カーソルキーの下を入力すれば一つ下の候補を選べます。
上にするには@<tt>{C-p} @<tt>{M-p} カーソルキーの上を入力します。
Enterを押すことでその補完候補を選択できます。
@<tt>{lisp-modeではあいまい補完を使っているので並びがあっていれば補完候補に表示されます。
補完機能はミニバッファでの入力などでも出来るので、例えばファイルを開く(}C-x C-f`)場合はファイル名が補完されます。


=== 式の評価とコンパイル


lispファイル上の式を評価してみます。
例としてfoo.lispというファイルを開きます。


//emlist[][lisp]{
;;; foo.lisp

(defun foo (x)
  (1+ x))
//}


関数fooの中で@<tt>{C-M-x}とすると、そのdefunが評価され、定義されます。



REPLで定義した関数を呼びだしてみます。


//emlist[][lisp]{
CL-USER> (foo 0)
1
//}


@<tt>{C-M-x}の代わりに@<tt>{C-c C-c}を使うと評価ではなくコンパイルされロードされます。
コンパイルするとコンパイル時の警告部分が赤線で引かれます。
//image[02-lem-compile][]{
//}




コンパイル時に出てきたウィンドウにの指定箇所にカーソルを合わせてEnterを押すことで特定箇所にジャンプすることが出来ます。
@<tt>{C-x C-n}、@<tt>{C-x C-p}を使って順番に特定箇所にジャンプすることも可能です。
赤線を消すには警告箇所を修正して再度@<tt>{C-c C-c}します。
@<tt>{C-c M-c}をしてもバッファ内の全ての赤線を消すことが出来ます。
ファイル自体を読み込むには@<tt>{C-c C-l}をします。
コンパイルし、その結果を読み込むには@<tt>{C-c C-k}をします。
カーソルの前の式を評価するには@<tt>{C-c C-e}をします。


//emlist[][lisp]{
(progn
  (foo) ; <- (foo)だけ評価するにはここでC-c C-e
  (bar))
//}


ミニバッファに式を入力し、評価するには@<tt>{C-c M-:}とします。
評価中の式が無限ループをして終わらない場合などはREPLでの中断とは別に@<tt>{C-c g}をして中断ができます。
評価は基本的に現在接続しているSWANKサーバで行いますが、その接続しているプロセスとは別にLemのカスタマイズをしたいなどの理由でLemのプロセス内で評価するコマンドも用意しています。
カーソルの前の式をLemプロセス内で評価する場合は@<tt>{C-c C-e}の代わりに@<tt>{C-x C-e}、
ミニバッファで入力した式をLemプロセス内で評価する場合は@<tt>{C-c M-:}の代わりに@<tt>{M-:}を使います。


=== マクロ展開


Common Lispではマクロ展開をするための関数としてmacroexpandがあり、一段階だけ展開したい場合の関数としてmacroexpand-1があります。
lemからマクロ展開をするには@<tt>{C-c C-m (M-x lisp-macroexpand)}を使います。
このコマンドは一段階だけマクロを展開します。
マクロ展開にはSWANKサーバ側でそのマクロを定義されている必要があるため、事前にマクロを評価しておく必要があります。
例としてhttp://clhs.lisp.se/Body/f@<b>{mexp}.htm のExamplesで定義されているマクロ@<tt>{alpha}, @<tt>{beta}でマクロ展開してみます。


//emlist[][lisp]{
(defmacro alpha (x y) `(beta ,x ,y))
(defmacro beta (x y) `(gamma ,x ,y))
//}


@<tt>{(alpha 1 2)}というマクロを一段階だけ展開したい場合は最初の括弧の位置にカーソルを置いて@<tt>{C-c C-m}します。
@<tt>{(alpha (beta 1 2) 3)}という式を全て展開したい場合は@<tt>{(alpha}の先頭の括弧にカーソルを置いて@<tt>{C-c M-m}します。



試してみると展開結果がTypeoutウィンドウに出力されます。
この展開結果で更にマクロ展開したい場合は同じようにカーソルを展開したいマクロの開き括弧に合わせて@<tt>{C-c C-m}とします。


=== 定義へのジャンプ


ある関数や変数、クラスなどの定義位置を参照する機能があります。
@<tt>{M-. (M-x find-definitions)}を使うことでソースコードの適当箇所にジャンプすることができます。
カーソル位置にシンボルがある場合はその定義位置へジャンプし、無ければミニバッファからシンボル名を入力します。
元の位置に戻るには@<tt>{M-, (M-x pop-definition-stack)}を使います。



この機能はSWANKサーバ側でシンボルを参照するので、事前にそのシンボルが定義されているようにシステムを読み込んでおかなければなりません。


//emlist{
CL-USER> (ql:quickload システム名)
//}


適当する定義が複数ある場合は定義箇所にジャンプする前に一覧が別ウィンドウに表示されます。
次の画像はcl-ppcre:scanを対象にした例です。
//image[02-lem-jump-to-definitions][]{
//}




この場合は@<tt>{C-x C-n} @<tt>{C-x C-p}で定義箇所に順番にジャンプできます。
一覧が表示されたウィンドウに移動して見たい定義にカーソルを合わせてEnterを押すことでも定義位置にジャンプできます。
この操作方法はコンパイラの警告の一覧と同じす。この機能は他にもgrepなどで使われています。


=== シンボルが使われている場所の一覧


ある関数や変数、クラスなどがどこで使われているかを参照したい場合があります。
@<tt>{M-_ (M-x find-references)}を使うことでそのシンボルの関数がどこから呼ばれているか、変数がどこから参照されているかを確認できます。
これは定義へのジャンプと使い方が同じで、事前にシステムの読み込みも同じく必要です。
違う点は定義側とそれを使っている側という点です。


=== Apropos


Common Lispで存在するシンボルを検索するにはaproposという関数を使います。


//emlist[][lisp]{
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
//}


SLIMEではエディタからaproposを使えるインターフェースを用意しています。

 * @<tt>{C-c C-d a (M-x lisp-apropos)}でexportされたシンボルの大文字小文字を区別しない検索
 * @<tt>{C-u C-c C-d a (C-u M-x lisp-apropos)}でミニバッファからexportされているか、大文字小文字を区別するかを選んで検索
 * @<tt>{C-c C-d z (M-x lisp-apropos-all)}で全てのパッケージの全てのシンボルを大文字小文字区別せずに検索
 * @<tt>{C-c C-d p (M-x lisp-apropos-package)}で指定したパッケージの全てのシンボルを表示



があります。



次の画像はcl-ppcreのscanを検索した例です。
//image[02-lem-apropos][]{
//}




この検索結果のシンボルの位置でReturnを押すと定義箇所へのジャンプが出来ます。


=== インスペクタ


Common Lispではオブジェクトの中身を見たり、必要ならその場で値を変更できるinspectという関数があります。
SLIMEではエディタから使うためのインターフェースを提供しています。



試しにlemのウィンドウをinspectしてみます。
RPELで取り出したい式を評価し、その評価結果の値をinspectしてみます。


//emlist{
CL-USER> (lem:current-window)
#<LEM::WINDOW {10052AEC83}>
//}


@<tt>{C-c I (M-x lisp-inspect)}でinspectを始められます。


//emlist{
Inspect Value (evaluated): *
//}


Common Lispでは最後にREPLで評価した値が@<tt>{*}に入るので、@<tt>{(lem:current-window)}の評価結果がinspectされます。



//image[02-lem-inspect-1][]{
//}




@<tt>{Tab}で次の選択できる位置までカーソルが移動され@<tt>{Enter}で選択でき、値を選択するとその値をさらにinspectできます。@<br>{}
左側の@<tt>{[ ]}を選択するとチェックがつき。その状態で[set value]を選択するとその値を変更することが出来ます。



inspectのコマンドの一覧についてはinspectバッファで@<tt>{M-x describe-bindings}を実行してください。



試しにlemのウィンドウの中のmodeline-formatを変更してみます。@<br>{}
modeline-foramtを選択しチェックを付け、@<tt>{[set value]}を選択するとミニバッファに次の入力画面が出ます。


//emlist[][lisp]{
Set slot LEM:MODELINE-FORMAT to (evaluated) :
//}


たとえば次のように入力するとREPLウィンドウのモードラインの見た目が変更されます。


//emlist[][lisp]{
Set slot LEM:MODELINE-FORMAT to (evaluated) : 
`(("-----------------HELLO WORLD------------------"
  ,(lem:make-attribute :foreground "red" :background "white" :bold-p t)))
//}


//image[02-lem-inspect-2][]{
//}




今動かしているエディタ自体をinspectで動的に変更することはエディタを簡単に壊せてしまいますが、とても魅力的な機能です。



他の使いどころとしてはhttp-requestで返ってきたレスポンスの中身を覗く場合に使います。@<br>{}
クラスのインスタンスやハッシュテーブルの中身はREPLの結果からは見えないので、inspectで表示し、さらにその中身をinspectすることで
オブジェクトブラウザとして使うことが出来ます。


=== SLDB(デバッガ)


SLIMEでのデバッガはSLDBと呼ばれています。@<br>{}
評価した式でエラーが起こった場合はSLDBが起動し、専用のsldbバッファが表示されます。



次の画像はREPLでエラーが出る式を評価してSLDBが出た例です。
//image[02-lem-sldb-1][]{
//}




alexandria:lastcarは引数にリストを期待していますが文字列を渡しているので型エラーが出ています。



sldbバッファの内容を一つずつ見ていきます。



まず一番上の


//emlist{
The value
  "foo"
is not of type
  LIST
   [Condition of type TYPE-ERROR]
//}


ですが、これはエラーメッセージとコンディションの型です。



次に


//emlist{
Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1005708413}>)
//}


これはエラー後にどういう対応をするかをユーザーに尋ねています。@<br>{}
Common Lispにはリスタートという機構があり、プログラムの例外時にユーザーに次に何をするかの判断を委ねることが出来ます。@<br>{}
この場合は0と1と2という選択があります。@<br>{}
SLDBではそのバッファで数字を押すか、カーソル位置を色が変えられて表示されている[...]に合わせてEnterを押すと選択できます。



最後に


//emlist{
Backtrace:
  0: (ALEXANDRIA.0.DEV:LASTCAR "foo")
  1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (ALEXANDRIA.0.DEV:LASTCAR "foo") #<NULL-LEXENV>)
  2: (EVAL (ALEXANDRIA.0.DEV:LASTCAR "foo"))
 --more--
//}


がありますが、フレームのリストを表示しています。@<br>{}
--more--と表示されている位置でEnterを押すと更に前の省略されているフレームも表示されます。@<br>{}
フレームの位置にカーソルを移動してEnterを押すとローカル変数などの詳細が表示されます。



@<tt>{Tab}で次の選択できる要素に移動します。



@<tt>{n}と@<tt>{p}で上下のフレームにカーソルを移動でき、@<tt>{M-n}と@<tt>{M-p}で移動と同時に詳細を表示できます。



デバッガは終了するには@<tt>{q}を押します。



sldbのコマンドの一覧についてはsldbバッファで@<tt>{M-x describe-bindings}を実行してください。


==== 実例


実際にsldbを使ってみましょう。



まずこのようなファイルがあるとします。


//emlist[][lisp]{
;;; example.lisp

(ql:quickload :cl-ppcre :silent t)

(defclass matcher ()
  ((pattern
    :initarg :pattern
    :reader matcher-pattern)
   (action
    :initarg :action
    :reader matcher-action)))

(defun match (matcher input)
  (if (ppcre:scan (matcher-pattern matcher) input)
      (funcall (matcher-action matcher) input)
      (error "match error")))

(defun convert-integer (input)
  (values (parse-integer input)))
//}


これはmatcherでルールに正規表現を使ってマッチしたらactionを呼び出すプログラムです。



デバッグをする場合はコンパイルオプションをデバッグ用に設定するのが良いです。
RPELで次の宣言をします。


//emlist{
CL-USER> (declaim (optimize (debug 3)))
//}


こうすることで、これからコンパイルするプログラムにはこのオプションが適用されるのでデバッグ時に参照できる情報が多くなります。



次にREPLから使うためにファイルをコンパイルします。
このファイルを開いているバッファで@<tt>{C-c C-k}をしてファイルのコンパイルと読み込みを行います。



//image[02-lem-sldb-practice-1][]{
//}




REPLからmatcherインスタンスを作ってmatch関数を使ってみます。


//emlist[][lisp]{
CL-USER> (defparameter m
           (make-instance 'matcher :pattern "\\d+" :action 'convert-integer))
m
CL-USER> (match m "123")
123
CL-USER> (match m "1000")
1000
//}


matcherを使ってmatch関数を呼び出すことで入力を数値に変換することができました。



次はエラーが出る例を試してみます。


//emlist[][lisp]{
CL-USER> (match m "123d")
//}


//image[02-lem-sldb-practice-2][]{
//}




2番目のフレームのconvert-integer関数内でエラーが起こってるみたいです。
sldbのフレームの位置で@<tt>{v (M-x sldb-show-frame-source)}をすると対応するソースの箇所に飛べます。
エラーの原因としてはparse-integerで"123d"を渡していることが原因のようですが、
ここでは"123d"も123に変換してほしいのでconvert-integerとは別に曖昧な入力も許す関数を追加します。


//emlist[][lisp]{
(defun convert-integer* (input)
  (values (parse-integer input :junk-allowed t)))
//}


ここでこの関数を使えるようにするためにこの関数を@<tt>{C-c C-c}でコンパイルします。



//image[02-lem-sldb-practice-3][]{
//}




sldbからでもinspectを使うことが出来ます。
新しく追加した関数をmatcherオブジェクトのactionにしてしまいましょう。



match関数内に対応する0から数えて2のフレームを選択し、詳細が表示されるので、引数のmatcherを選択するとinspectが起動します。



//image[02-lem-sldb-practice-4][]{
//}




ここで、inspectの項でスロットの変更を行なったのと同じようにmatcherのスロットのactionを@<tt>{'convert-integer*}に変更します。
このあとinspectを@<tt>{q}で終了し、同じ2番目のフレーム内で@<tt>{r (M-x sldb-restart-frame)}をすると、そのフレームからリスタートが行なわれます。
これでエラーが起こらずに値を変換できました。@<br>{}
REPLでも結果が返ってきていることを確認できます。



//image[02-lem-sldb-practice-5][]{
//}




このようにCommon Lispではデバッガやinspectを使って動作中のプログラムを変更し、リスタートを行うことで動的に開発を行えます。


== 拡張機能の書き方


lemでの拡張機能の書き方を紹介します。プロジェクト名はposts-listとして、redditの投稿一覧のビューアを作ります。



完成は次のようになります。



//image[02-extension-preview][完成図]{
//}




上のスクリーンショットでは/r/lispの投稿のリストを取得して表示しています。
それぞれの行の[...]が投稿者名でそのあとのテキストがタイトルです。
選択するとリンク先をブラウザで開く機能などがついています。



本書が白黒で印刷された場合の注意書きですが、投稿者名のところは赤で、タイトルは水色で表示されています。


=== プロジェクトの作成


lemの拡張機能は、他のプロジェクトと同様に、ASDFのシステムとして扱います。ベースとするディレクトリの下に、拡張機能と同じ名前のディレクトリを用意し、その下に各ファイルを配置します。以下のコマンドでプロジェクトのテンプレートを生成します。


//emlist{
$ ros init lem posts-list
//}


これでプロジェクトのベースが出来ました。ディレクトリツリーは以下のようになります。


//emlist{
$ tree posts-list
posts-list
├── lem-posts-list.asd
└── main.lisp
//}


@<tt>{M-x start-lisp-repl}でREPLを起動して、作成したプロジェクトを読み込みます。


//emlist[][lisp]{
CL-USER> (ql:quickload :lem-posts-list)
//}


もしエラーが出て読み込めない場合、パスが通っていない可能性が高いです。デフォルトでは@<tt>{$HOME/common-lisp}にパスが通ってあるので、posts-listディレクトリを@<tt>{$HOME/common-lisp/}以下に配置してみてください。


=== 投稿リストを取得


subredditを指定して、redditの投稿をjsonで取得します。1つの投稿をpostという構造体にして、postのリストを返す処理を用意します。これ自体はlemとは関係ないので@<tt>{posts.lisp}に分離します。


//emlist[][lisp]{
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
//}


lem-posts-list/postsというパッケージに分離しました。fetch-postsという関数とpostのアクセサを外部から使いたいのでexportしています。



関数fetch-postsは引数でsubreddit名を受け取り返り値はpostのリストです。REPLから動作を確認してみます。


//emlist[][lisp]{
CL-USER> (ql:quickload :lem-posts-list/posts)
CL-USER> (lem-posts-list/posts:fetch-posts "lisp")

;; 出力は長いので省略
//}


投稿一覧が取得できるようになったので、次はその内容をLemに表示し選択できるようにしますが、その前にLemで扱うオブジェクトについていくつか見ていきます。


=== buffer


バッファはテキストとその色やカーソルの位置、モードなどが入ったオブジェクトです。通常はファイルを開くときにバッファはそのファイルと関連付けられますが、ファイルと関連付けずにバッファ自体を作成することも可能です。



バッファは@<tt>{make-buffer}関数を使うことで作成できます。ファイルと関連付けられたバッファは@<tt>{find-file-buffer}関数を使うことで作成できます。


//emlist[][lisp]{
(lem:make-buffer "test") ; => #<BUFFER test NIL>
(lem:find-file-buffer "/tmp/hoge") ; => #<BUFFER hoge /tmp/hoge>
//}

=== point


ポイントはバッファ内の位置を指すオブジェクトです。主にカーソルなどに使われています。バッファ内への文字列の挿入や削除に使います。ポイントを扱う場合はバッファ内に既にあるポイントをコピーして使う事が多いです。バッファからポイントを得るアクセサは次のようなものがあります。


//emlist[][lisp]{
;; バッファの現在の位置のポイントを得る
(buffer-point buffer)

;; バッファの先頭の位置のポイントを得る
(buffer-start-point buffer)

;; バッファの末尾の位置のポイントを得る
(buffer-end-point buffer)
//}


ポイントのコピーには@<tt>{copy-point}関数を使います。


//emlist[][lisp]{
(copy-point point &optional kind)
//}


ポイントはスティッキーな動作をします。そのポイントより前の位置に文字列を挿入するとその分右へずれていき、削除すると左にずれていきます。



@<tt>{kind}はバッファ編集時のオフセットを計算するときに使います。



@<tt>{kind}が:left-insertingならポイントと同じ位置に文字列を挿入したときに右にずれ、:right-insertingならそのままです。



@<tt>{kind}が:temporaryの場合は何も行いません。



@<tt>{kind}を指定しなければ渡された@<tt>{point}と同じ値になります。



@<tt>{kind}が:temporary以外ならpointをbufferが保持しておく必要があるので不要になったら明示的に削除しなければいけません。
削除には@<tt>{delete-point}関数を使います。


//emlist[][lisp]{
(let ((point (copy-point (buffer-point buffer) kind)))
  (unwind-protect ...
    (delete-point point)))
//}


このために@<tt>{with-point}マクロを用意しています。


//emlist[][lisp]{
(with-point ((point (buffer-point buffer) kind))
  ...)
//}


@<tt>{with-point}の@<tt>{kind}を省略した場合は:temporaryになります。


=== モード


バッファ上で操作をするときに、ソースコードを表示する場合はその言語専用の振舞いをしてほしい場合があります。
モードではそのモードの名前や対応するキーバインド、シンタックステーブル、などを設定します。



モードにはメジャーモードとマイナーモードがあり、メジャーモードはバッファに一つだけ設定でき、マイナーモードは複数設定できます。


=== キーマップ


キーを入力したときに特定のコマンドを呼び出すために、キーとコマンドの対応表をキーマップとして管理し、モードと関連付けて使われます。
キーマップは関数@<tt>{make-keymap}で作れますが、後述する@<tt>{define-major-mode}を使うと自動的に作られるようになっています。


=== posts-list-mode


では実際にLemに投稿の一覧を表示していくコードを書いていきます。



投稿の一覧を表示するための専用のバッファを用意し、そのバッファのメジャーモードをposts-list-modeに設定します。



メジャーモードを定義するにはdefine-major-modeを使います。


//emlist[][lisp]{
(define-major-mode posts-list-mode nil
  (:name "Posts list"
   :keymap *posts-list-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))
//}


define-major-modeのシンタックスは次のとおりです。


//emlist[][lisp]{
(define-major-mode モード名 継承する親モード
  (:name モードラインに表示する名前
   :keymap キーマップ
   :syntax-table シンタックステーブル)
  本体...)
//}


@<tt>{:name} @<tt>{:keymap} @<tt>{:syntax-table} は省略可能です。
本体はモードが有効になったタイミングで使われます。
今回は@<tt>{:name}と@<tt>{:keymap}を使います。
@<tt>{:keymap}には@<tt>{*posts-list-mode-keymap*}を指定しています。
:keymapを指定すると自動的にキーマップが作られ、指定した名前のスペシャル変数が定義されます。
また投稿一覧が表示されているバッファをユーザーが変更できないようにしたいので読み込み専用にします。



本体に


//emlist[][lisp]{
(setf (buffer-read-only-p (current-buffer)) t)
//}


と書くことでモードが有効になったときに現在のバッファを読み込み専用になるようにします。


=== 色の定義


投稿者名とタイトルの色を設定するために、その色の定義をします。
バッファ内のテキストの見た目はattributeというオブジェクトで扱っていて、
attributeの定義は@<tt>{define-attribute}を使います。


//emlist[][lisp]{
(define-attribute author-attribute
  (t :foreground "red"))

(define-attribute title-attribute
  (:light :foreground "blue")
  (:dark :foreground "cyan"))
//}


author-attributeは投稿者に対応するattributeでforegroundをredにしています。
title-attributeはタイトルに対応し、Lemで設定している背景色が明るい色ならforegroundをblue、暗い色ならcyanにします。



文字列を挿入するときにattributeを指定すると、色の付いた文字列になります。


//emlist[][lisp]{
(insert-string (current-point) "Hello World" :attribute 'attribute-name)
//}

=== 投稿一覧バッファを作る


fetch-postsで得たpostのリストをバッファに書き込む関数を作ります。


//emlist[][lisp]{
(defun write-post (point post)
  (with-point ((start point :right-inserting))
    (insert-string point
                   (format nil "[~A]" (post-author post))
                   :attribute 'author-attribute)
    (insert-string point " ")
    (insert-string point
                   (post-title post)
                   :attribute 'title-attribute)
    (insert-character point #\newline)
    (put-text-property start point :post post)))

(defun write-posts (point posts)
  (dolist (post posts)
    (write-post point post)))
//}


write-postは一つのpostを受け取り、それを一行の内容としてattributeを指定して色を付けながらバッファに書き込んでいます。
write-postsはfetch-postsから返ってくる値に合わせ、postのリストを受け取りバッファに書き込みます。
次は投稿一覧バッファを作る処理です。


//emlist[][lisp]{
(defun make-posts-list-buffer (subreddit)
  (let ((posts (fetch-posts subreddit))
        (buffer (make-buffer (format nil "*Reddit ~A*" subreddit))))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (let ((point (buffer-point buffer)))
        (write-posts point posts)
        (buffer-start point)))
    buffer))
//}


引数にsubreddit名を受け取り、返り値は投稿一覧が書き込まれたバッファです。
make-bufferは既に同じ名前のバッファがあるならそれを返し、無ければ作ります。
with-buffer-read-onlyのシンタックスは次のとおりです。


//emlist[][lisp]{
(with-buffer-read-only buffer read-only-p
  &body body)
//}


bufferのread-onlyフラグを引数の値に変更した後body内を実行し終わったら元に戻します。
普段は読込専用のバッファを一時的に編集するためのイディオムです。


=== コマンドの定義


最後に見たいsubredditを入力し、その投稿一覧バッファを表示するコマンドを作ります。


//emlist[][lisp]{
(define-command posts-list (subreddit) ("sSubreddit: ")
  (let ((buffer (make-posts-list-buffer subreddit)))
    (switch-to-buffer buffer)
    (change-buffer-mode buffer 'posts-list-mode)))
//}


これで@<tt>{M-x posts-list}で呼び出せるようになります。
define-commandのシンタックスは次のとおりです。


//emlist[][lisp]{
(define-command command (&rest arguments) (&optional arg-descriptor)
  &body body)
//}


基本的にはdefunと同じですが、三つ目の引数のarg-descriptorを追加で指定する必要があります。
"p"を渡した場合、コマンド実行前に(C-u 数字)を入力した値が引数に渡され、デフォルト値は1になります。
"P"だと"p"と同じですが、デフォルト値がnilになります。
"sプロンプト"はコマンドを実行前にミニバッファで入力が促され、入力した文字列が引数に渡されます。
define-commandでコマンドを追加するとM-xで呼び出せるようになり、キーにも束縛できるようになります。


=== キーバインドとコマンドの追加


バッファに表示ができるようになったので、次は投稿を選択しブラウザで表示するようにしてみます。
その行の投稿をブラウザで表示するコマンドを作り、Returnキーにバインドさせます。


//emlist[][lisp]{
(define-command posts-list-select () ()
  (let ((post (text-property-at (current-point) :post)))
    (open-browser (post-url post))))
//}


その行の:postプロパティを取得し、それに対応するurlをブラウザで開きます。
キーバインドの追加は次のようにします。


//emlist[][lisp]{
(define-key *posts-list-mode-keymap* "Return" 'posts-list-select)
//}


一つ目の引数がkeymapで、二つ目がキー、三つ目がコマンドになります。


=== 完成


これで最低限のredditの投稿を表示する機能が出来上がりました。
他に一つの投稿内のコメント一覧を表示するバッファを作ることなど、やっていないことはまだまだあります。
他の拡張機能の例としてLemのリポジトリ内のcontrib/以下にもあるものやmodes/以下にあるものが参考になるかもしれません。

