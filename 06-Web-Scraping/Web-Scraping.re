= Webスクレイピングと文書分類

== この章でやること

インターネット上を流れるテキストデータは日々ものすごいスピードで増え続けており、人手で追い続けるのはもはや不可能です。また、それらのテキストデータは大抵の場合構造化されておらず、そのままでは使えないことが多いため、データ分析が必要になります。テキストデータをデータ分析し、意味ある構造を取り出す行為をテキストマイニングと呼びます。

本章では、Common Lispのライブラリを用いて、インターネットをクローリングし、必要な情報を収集し、その上で集めたテキストデータを自動分類するための方法を紹介します。

== Webスクレイピング

=== Webスクレイピングに必要なライブラリ

WebスクレイピングとはWebサイトから情報を取り出す行為のことをいいます。
Webスクレイピングでは、(1) Webからデータを取得し、(2) これを解析し、扱いやすいデータ構造を作り、(3) そこから必要な情報を探索するというプロセスを踏みます。
Common Lispにはこの各段階に対応するライブラリがあり、以下ではそのインストール方法と基本的な使用例を解説していきます。

=== HTTPクライアント: Dexador

Webサイトからデータを取得するためにはHTTPクライアントの@<b>{Dexador}(デキサドル)を使用します。Common Lispには古くからDrakmaというHTTPクライアントがありますが、Dexadorの方が(特に同じホストに複数回アクセスする場合において)速く動作します。

==== インストール

インストールはQuicklispから行います。

//emlist{
(ql:quickload :dexador)
//}

==== 使い方

Dexadorの最も基本的な関数は@<code>{dex:get}で、HTTPのGETメソッドで指定のURLからデータを取得します。次のコードは@<tt>{http://lisp.org/}にアクセスし、そこから取得したHTMLデータを文字列として@<code>{*html-source*}に設定します。

//emlist{
(defparameter *html-source* (dex:get "http://lisp.org/"))
//}


@<code>{get}関数は(1) 本体データの文字列、(2) HTTPステータスコード、(3) レスポンスヘッダーのハッシュテーブル、(4) URI構造体、(5) 読み出し元ソケットのストリームの5つを多値で返します。以下は上記で実行した@<code>{get}関数の全ての返り値を列挙したものになります。

//emlist{
(1) 本体データの文字列
"<HTML>
<HEAD>
  <title>John McCarthy, 1927-2011</title>
  <STYLE type=\"text/css\">
    BODY {text-align: center}
  </STYLE>
</HEAD>
<BODY>
<h1>John McCarthy</h1>
<img src=\"jmccolor.jpg\" alt=\"a picture of John McCarthy, from his website\"/>
<h3>1927-2011</h3>
<br><br>
<a href=\"http://www-formal.stanford.edu/jmc/\">John McCarthy's Home Page</a><br>
<a href=\"http://news.stanford.edu/news/....html\">Obituary</a>
</BODY>
</HTML>
"
(2) HTTPステータスコード
200
(3) レスポンスヘッダーのハッシュテーブル
#<HASH-TABLE :TEST EQUAL :COUNT 10>
(4) URI構造体
#<QURI.URI.HTTP:URI-HTTPS https://lisp.org/>
(5) 読み出し元ソケットのストリーム
#<CL+SSL::SSL-STREAM for #<FD-STREAM>>
//}


このうち最初のデータ本体は次節で扱うHTMLパーサへの入力として用います。また、HTTPステータスコードはデータの取得に成功したか失敗したか、さらにはその理由を調べるのに役に立ちます。

=== XML/HTMLパーサ: Plump

ここで紹介する@<b>{Plump}と次節で紹介する@<b>{CLSS}は同じ作者のプロダクトで、セットで使われるものです。これらはDexadorと同様にQuicklispからインストールできます。以下のように、@<tt>{ql:quickload}には複数のパッケージ名をリストとして与えることもできます。


//emlist{
(ql:quickload '(:plump :clss))
//}


PlumpはXML/HTMLパーサであり、XML/HTMLデータを文字列として受け取り、CLOSオブジェクトのノードから構成される木構造(DOMツリー)を生成し、そのルートノードのオブジェクトを返します。

例えば以下のようにして、前述のDexadorの@<tt>{get}の結果をパースすると、ルートノードのオブジェクトが返ります。


//emlist{
(defparameter *root-node* (plump:parse *html-source*))

;; *root-node* => #<PLUMP-DOM:ROOT>
//}


各ノードの子ノードのベクタは@<tt>{plump:children}で、親ノードは@<tt>{plump:parent}で得られます。

//emlist{
(plump:children *root-node*)
;; => #(#<PLUMP-DOM:ELEMENT HTML> #<PLUMP-DOM:TEXT-NODE>)

(plump:parent (aref (plump:children *root-node*) 0))
;; => #<PLUMP-DOM:ROOT>
//}


また、DOMツリーを走査し、各ノードに対して関数を適用する高階関数@<tt>{plump:traverse}が用意されています。
例えば、テキストノードから文字列を取り出して連結して返す関数は以下のようになります。


//emlist{
(defun concat-node-text (node)
  (let ((text-list nil))
    (plump:traverse node
                    (lambda (node) (push (plump:text node) text-list))
                    :test #'plump:text-node-p)
    (apply #'concatenate 'string (nreverse text-list))))
//}


@<tt>{traverse}にはDOMツリーの各ノードに適用される関数を渡します。キーワードパラメータ@<tt>{:test}に述語関数を渡すと、それを満足するノードに対してのみ関数が適用されます。ここでは述語関数@<tt>{plump:text-node-p}を渡しているので、テキストデータを持つノードに対してのみ関数が適用され、そのノードの持つ文字列をリストに@<tt>{push}していっています。最後に@<tt>{nreverse}で順序を反転させ、@<tt>{concatenate}で連結して返すという関数になっています。


この関数の評価結果は以下のようになるでしょう。

//emlist{
(concat-node-text *root-node*)

"John McCarthy, 1927-2011
John McCarthy
1927-2011
John McCarthy's Home Page
Obituary"
//}


同様にして、取得したDOMツリーに対して変更を加えることもできます。例えばテキストノードの文字列を全て大文字に変更するには、@<tt>{traverse}でDOMツリーを走査しながら、@<tt>{text-node-p}を満足するノードオブジェクトのスロットに@<tt>{setf}で新しい値を設定すればよいということになります。

//emlist{
(plump:traverse *root-node*
                (lambda (node)
                  (setf (plump:text node)
                        (string-upcase (plump:text node))))
                :test #'plump:text-node-p)
//}

これで@<code>{*root-node*}以下のDOMツリーを破壊的に変更されたので、@<code>{concat-node-text}の結果は大文字に置き換わっています。

//emlist{
(concat-node-text *root-node*)

"JOHN MCCARTHY, 1927-2011
JOHN MCCARTHY
1927-2011
JOHN MCCARTHY'S HOME PAGE
OBITUARY"
//}

以上のように、Plumpを使えばHTMLからDOMツリーを生成し、そこから情報を取り出したり変更を加えることができます。


=== CSSセレクタ: CLSS


次はDOMツリーから欲しい情報を取り出す方法を紹介します。
先ほどPlumpと一緒にインストールした@<b>{CLSS}はCSSセレクタと呼ばれるもので、XML/HTMLのタグやCSSクラス、IDなどを使ってDOMツリーからノードを検索することができます。


==== タグ名による検索


例えばIMGタグでルートノード以下を検索すると、Plumpのノードのベクタが得られます。その最初の要素を変数@<tt>{*img-node*}に持っておくことにします。


//emlist{
(defparameter *img-nodes* (clss:select "img" *root-node*))

;; *img-nodes* => #(#<PLUMP-DOM:ELEMENT img>)

(defparameter *img-node* (aref *img-nodes* 0))
//}


このノードを@<tt>{describe}してみると、子ノードを持っておらず、タグ名はIMGで、属性の情報をハッシュテーブルとして持っていることが分かります。

//emlist{
(describe *img-node*)

;; Slots with :INSTANCE allocation:
;;   %PARENT                        = #<PLUMP-DOM:ELEMENT BODY>
;;   %CHILDREN                      = #()
;;   %TAG-NAME                      = "img"
;;   %ATTRIBUTES                    = #<HASH-TABLE :TEST EQUALP :COUNT 2>
//}


なお、@<tt>{plump:attributes}でノードオブジェクトから属性のハッシュテーブルを取得でき、各属性値には@<tt>{plump:attribute}でアクセスできます。このハッシュテーブルの中身を表示してみると、属性値としてsrcとaltを持っていることが分かります。


//emlist{
(defun print-node-attributes (node)
  (maphash (lambda (key value)
             (format t "key: ~A, value: ~A~%" key value))
           (plump:attributes node)))

(print-node-attributes *img-node*)

;; key: src, value: jmccolor.jpg
;; key: alt, value: a picture of John McCarthy, from his website

(plump:attribute *img-node* "alt")

;; "a picture of John McCarthy, from his website"

(plump:attribute *img-node* "src")

;; "jmccolor.jpg"
//}


ここまでに説明したことを用いれば、例えば、全てのAタグを検索し、リンク先を列挙するようなことができます。

//emlist{
(loop for a-node across (clss:select "a" *root-node*)
      collect (plump:attribute a-node "href"))

;; ("http://www-formal.stanford.edu/jmc/"
;;  "http://news.stanford.edu/news/2011/october/john-mccarthy-obit-102511.html")
//}

==== CSSクラス、IDによる検索


次に、CSSのクラスやIDをキーとして、DOMツリーからノードを検索する例を紹介します。まず例として、Clojureの公式サイトを取得します。


//emlist{
(defparameter *clojure-root-node*
  (plump:parse (dex:get "https://clojure.org/")))
//}


Chromeなどの最近のWebブラウザにはインスペクタが搭載されており、Webページ上の調べたいところを右クリックして「検証」や「@<tt>{Inspect}」などと表記されている項目を選べば、HTMLソース中の対応する部分がハイライトされるようになっています。

このサイトをブラウザのインスペクタで調べると、内容のまとまりで@<tt>{w-section}というクラスが付いていることが分かります(@<img>{06-clojure})。

//image[06-clojure][Chrome上でインスペクタを起動した画面][scale=1.1]{
//}

CLSSを使えば、このクラス名でDOMツリーを検索することができます。なお、CSSと同様に、クラスの場合はクラス名の前に@<tt>{.}を付け、IDの場合はID名の前に@<tt>{#}を付けることで指定できます。また、タグとの組み合わせで指定することもできます。


//emlist{
(clss:select ".w-section" *clojure-root-node*)

;; #(#<PLUMP-DOM:ELEMENT div> #<PLUMP-DOM:ELEMENT div> #<PLUMP-DOM:ELEMENT div>
;;   #<PLUMP-DOM:ELEMENT div> #<PLUMP-DOM:ELEMENT div> #<PLUMP-DOM:ELEMENT div>
;;   #<PLUMP-DOM:ELEMENT div> #<PLUMP-DOM:ELEMENT div>)

(clss:select "div.clj-intro-message" *clojure-root-node*)

;; #(#<PLUMP-DOM:ELEMENT div>)
//}

=== スクレイピングの実際


ここまでに説明したことを用いれば、様々なWebサイトから情報を取り出すことができます。@<tt>{dex:get}でWebサイトのURLからHTMLを文字列として取得し、@<tt>{plump:parse}でCLOSオブジェクトのノードから成るDOMツリーを作り、@<tt>{clss:select}で所望のノードを取り出します。その後は@<tt>{concat-node-text}や@<tt>{plump:attribute}などで必要な値を取り出すという流れになります。

Chromeの場合、インスペクタのHTMLソースから、クラスやID、タグといったものを調べられます。また、HTMLソースの要素上で右クリックして「@<tt>{Copy > Copy selector}」を選択することで、その要素に対応するCSSセレクタをクリップボードにコピーできます。これをそのまま@<tt>{clss:select}に渡せば該当部分に対応するノードを得られます。

例えば、先程のページでClojureのロゴマーク上でインスペクタを起動し、@<tt>{Copy > Copy selector}でコピーした文字列をCLSSに与えると、その部分のノードが得られます。

//emlist{
(clss:select "body > div.w-nav.clj-navbar > div.w-container > a > img" *clojure-root-node*)

;; #(#<PLUMP-DOM:ELEMENT img>)
//}

Chromeにはインスペクタの他にもHTTPリクエストとそのレスポンスを記録する機能などもあり、それらは@<b>{DevTools}という画面にまとまっています。DevToolsは右上のメニューから「@<tt>{More tools > Developer tools}」を選択するか、キーボードから@<tt>{Ctrl + Shift + I}を入力することで開くことができます。

=== ログインを必要とするサイト

ここまでのところはデータの取得は@<tt>{dex:get}のみで事足りていましたが、ログインを必要とするサイトではフォームからログイン情報を送信し、さらにその後のセッション管理が必要になるため、事情はやや複雑になります。

Dexadorには@<tt>{dex:get}の他にも、@<tt>{dex:post}や@<tt>{dex:request}などのリクエスト関数があり、ほとんど同じように使えます。これらのリクエスト関数にはCookieオブジェクトを渡すことができ、これを使ってログイン状態を保持することができます。
ログインの全体の流れとしては、まず空のCookieオブジェクトを生成し、それを@<tt>{dex:post}に渡してログインフォームをサーバに送信することでCookieに値が設定されます。以降では同じCookieオブジェクトを@<tt>{dex:get}などに渡すとログインした状態でHTMLデータが取得できるようになります。

ここでは例として、はてなブックマークの「マイホットエントリー」のページを取得することを考えます。これははてなIDに連携されたSNSなどの情報を元に、各ユーザに合わせたおすすめ記事を表示するページで、ログインした状態でなければ見ることはできません。したがって、Cookieのない状態で、以下のはてなID@<tt>{(masatoi})のマイホットエントリーのページを取得してみると、アクセス権限が無いというエラーメッセージが返ります。


//emlist{
(dex:get "http://b.hatena.ne.jp/masatoi/hotentry")

;; An HTTP request to "http://b.hatena.ne.jp/masatoi/hotentry" returned 403 forbidden.
//}


次に、はてなのログインフォームにどのような情報を送るべきかを調べる必要があります。これにはChromeなどを使い実際にログインしてみて、開発ツール(DevTools)を使ってどのようなデータがPOSTされているのかを調べるのがよいでしょう。

まず、ブラウザではてなのログインページ(@<tt>{https://www.hatena.ne.jp/login})を開き、DevToolsからNetworkタブを開きます。この画面はHTTPリクエストやそれに対するレスポンスが記録される画面です。ログイン成功時には、はてなのトップページへリダイレクトされるので、ページの遷移や再読み込みでログが消えないように「@<tt>{Preserve log}」チェックボックスを選択しておく必要があることに注意が必要です。

DevToolsのNetworkタブを開いた状態で、Webページ上のログインフォームにログイン情報を入力して送信すると、たくさんのログが表示されます。ここからログイン情報を送っているPOSTリクエストを探します。左上の検索ウインドウからメソッドを指定してリクエストを検索することができるので、@<tt>{method:POST}と入力するとPOSTリクエストが検索できます。

//image[06-hatena-login-network][DevTools上でNetworkタブを選択しPOSTメソッドのHTTPリクエストを表示した画面][scale=1.1]{
//}

以上の操作をした時点での画面が@<img>{06-hatena-login-network}です。ここにはPOSTメソッドで送信しているフォームデータが表示されています(パスワードは平文で表示されているので隠してあります)。DexadorでPOSTリクエストを送るときはこのフォームデータの形式に従う必要があります。


次のコードでは、まず@<tt>{cl-cookie:make-cookie-jar}でCookieオブジェクトを生成し、それを@<tt>{dex:post}に渡してユーザ名とパスワードとともに送ります。そうすると@<tt>{*cookie-jar*}に値が設定されるので、それを@<tt>{dex:get}に渡して先程のマイホットエントリーのページを取得すると、今度はHTMLデータが取得できるようになっています。

//emlist{
(defparameter *hatena-login-url* "https://www.hatena.ne.jp/login")
(defparameter *cookie-jar* (cl-cookie:make-cookie-jar))

(dex:post *hatena-login-url*
          :cookie-jar *cookie-jar*
          :content '(("name" . "masatoi")
                     ("password" . "hogehoge")))

(dex:get "http://b.hatena.ne.jp/masatoi/hotentry" :cookie-jar *cookie-jar*)

;; => "<!DOCTYPE html> <html lang=\"ja\" ... </html>"
;; => 200
;; => #<HASH-TABLE :TEST EQUAL :COUNT 14>
;; => #<QURI.URI.HTTP:URI-HTTP http://b.hatena.ne.jp/masatoi/hotentry>
;; => #<SB-SYS:FD-STREAM for "socket 192.168.xx.yy:zz, peer: 13.35.50.30:80">
//}


ログインと同様に、Webサイトからログアウトするときもログアウト用のURLにPOSTリクエストを送るだけです。以下のように、ログインして一連の処理を行い、最後に必ずログアウトするようなマクロ@<tt>{with-hatena}を考えることができます。


//emlist{
(defparameter *hatena-logout-url* "https://www.hatena.ne.jp/logout")

(defmacro with-hatena ((cookie-jar) (&key id password) &body body)
  `(let ((,cookie-jar (cl-cookie:make-cookie-jar)))
     (dex:post *hatena-login-url*
               :cookie-jar ,cookie-jar
               :content `(("name" . ,,id)
                          ("password" . ,,password)))
     (unwind-protect
          (progn ,@body)
       (dex:post *hatena-logout-url* :cookie-jar ,cookie-jar))))
//}


このマクロ内で生成されるCookieオブジェクトは局所変数に束縛され、本体部分でのみ使用することができます。@<tt>{with-hatena}マクロを使うことで、ログイン/ログアウト処理を意識することなくログインが必要なURLにアクセスすることができます。


//emlist{
(with-hatena (cookie-jar) (:id "masatoi" :password "hogehoge")
  (dex:get "http://b.hatena.ne.jp/masatoi/hotentry" :cookie-jar cookie-jar))
//}


実際のところ、ログインのためにJavaScriptを必要とするサイトや、ユーザIDとパスワード以外にCSRFトークンなどを必要とするサイトも多く、サイト毎にログイン方法は異なります。しかし大枠としては、ここで解説したような流れでログインすることができるでしょう。


== 文書分類

この節では、Webスクレイピングにより大量のテキストデータが得られたと仮定して、そこから機械学習アルゴリズムを使ってどのように構造を取り出すかについて解説します。
ここでは特に、文書分類について解説します。

文書分類は教師あり学習の問題です。文書に対してあらかじめラベル(記事カテゴリやスパムか否かなど)が付いているデータで学習し、未知の文書に対してその文書がどのラベルかを推定します。例えば、メールソフトのスパムフィルタや自動タグ付けががこれに当たります。

Common Lispには機械学習ライブラリの数こそ少ないですが、外部ライブラリに頼らなくても高速な数値計算ができるので、こういったデータ分析には向いているといえます。

=== 文書分類に必要なライブラリ

日本語の文書を分析するためには、まず文書から単語を切り出す必要があります。分かち書きされていない文から語句を切り出し、それぞれに品詞を付けることを@<b>{形態素解析}といいます。
オープンソースで公開されている有名な形態素解析エンジンとして@<b>{MeCab}があります。Common LispにはMeCabの辞書を利用し、推論自体はピュアCommon Lispで行う@<b>{cl-igo}というライブラリがあります。

文書分類の学習部分には線形分類器を使用します。Common Lispで実装された高速な線形分類器としては@<b>{cl-online-learning}があります。cl-online-learningは割と最近のアルゴリズムもカバーしており、スパースなデータにも対応しています。スパース(疎)なデータとは、入力値の大部分が0であるようなデータのことで、文書をスパースデータとして表現する方法についても後に解説します。

=== 形態素解析器: cl-igo

cl-igo@<fn>{igo-url}はCommon Lispから使える形態素解析器で、MeCabの辞書を変換したバイナリ辞書を使います。辞書の変換にはJavaを使用しますが、その後の推論部分はCommon Lispのみで実装されています。

//footnote[igo-url][http://igo.osdn.jp/cl-igo.html]


==== cl-igoのインストール


RoswellからインストールできるようにGitHubにミラーを作ったので、以下のようにしてインストールすることができます。


//cmd{
$ ros install masatoi/charseq masatoi/cl-igo
//}

==== cl-igoの辞書を用意する


次にバイナリ辞書を作ります。バイナリ辞書のビルドにはJava版のIgoが必要になります。ここではJavaのランタイムは既にインストールされているものとします。
以下のURLからMeCabの辞書データ(@<tt>{mecab-ipadic-2.7.0-20070801.tgz})と、Igoのjarファイル(@<tt>{igo-0.4.5.jar})をダウンロードします。

 * @<tt>{https://sourceforge.net/projects/mecab/files/mecab-ipadic/2.7.0-20070801/}
 * @<tt>{https://osdn.net/projects/igo/downloads/55029/igo-0.4.5.jar/}

ここでダウンロードした2つのファイルと同じディレクトリで次のコマンドを実行すると、@<tt>{~/igo/ipadic}というディレクトリにバイナリ辞書ができます。


//cmd{
$ tar xzvf mecab-ipadic-2.7.0-20070801.tar.gz

$ java -cp ./igo-0.4.5.jar net.reduls.igo.bin.BuildDic ~/igo/ipadic \
mecab-ipadic-2.7.0-20070801 EUC-JP
//}

==== cl-igoをロードし、形態素解析を実行する

最後に、LispのREPLなどで@<tt>{ql:quickload}でcl-igoをロードし、@<tt>{igo:load-tagger}で先程作ったバイナリ辞書のディレクトリを指定して読み込みます。
以降は@<tt>{igo:parse}関数に日本語の文を文字列として与えれば、それを形態素解析した結果のリストが返ります。


//emlist{
;; cl-igoをロードする
(ql:quickload :igo)

;; 辞書を読み込む
(igo:load-tagger "~/igo/ipadic/")

;; 形態素解析を実行する
(igo:parse "庭には二羽にわとりがいる。")

'(("庭" "名詞,一般,*,*,*,*,庭,ニワ,ニワ" 0)
  ("に" "助詞,格助詞,一般,*,*,*,に,ニ,ニ" 1)
  ("は" "助詞,係助詞,*,*,*,*,は,ハ,ワ" 2)
  ("二" "名詞,数,*,*,*,*,二,ニ,ニ" 3)
  ("羽" "名詞,接尾,助数詞,*,*,*,羽,ワ,ワ" 4)
  ("にわとり" "名詞,一般,*,*,*,*,にわとり,ニワトリ,ニワトリ" 5)
  ("が" "助詞,格助詞,一般,*,*,*,が,ガ,ガ" 9)
  ("いる" "動詞,自立,*,*,一段,基本形,いる,イル,イル" 10)
  ("。" "記号,句点,*,*,*,*,。,。,。" 12))
//}

=== データセットの作成

==== Livedoorニュースコーパス

ここでは説明のために、実際にWebスクレイピングで集めたデータの代わりに、クリエイティブコモンズライセンスで公開されているLivedoorニュースコーパス@<fn>{livedoor}を使用します。

//footnote[livedoor][https://www.rondhuit.com/download.html#ldcc]

このデータセットは9つのニュースサイトから合計7367個の記事を集めたものであり、出典ごとにディレクトリに分け、1記事1ファイルのテキストファイルとして収録されています。以降では、ここでダウンロードしたファイルを@<tt>{~/datasets/livedoor}に展開したものとして話を進めます。

==== 文書データのスパースベクトルとしての表現

機械学習アルゴリズムを使ってデータから学習するには、まずデータの特徴量を適切に定める必要があります。


文書集合を構成する各文書は、1つの実数ベクトルとして表現できます。表現方法としては色々なものが考えられますが、文書分類の場合は文書中に出現する単語の頻度に注目した表現方法がよく使われます。具体的には、文書集合全体に現れる全単語数分の長さを持つベクトルを用意し、そのベクトルの各単語に対応する位置に、出現した回数を記録してきます。


文書集合全体に現れる単語数は3万語以上ありますが、1つの文書の中に現れる単語数は高々2、3百個でしかありません。そのため、文書に対応するベクトルはほとんどが値0の疎なベクトル(スパースベクトル)になります。このような場合、0でない要素のインデックスと値だけを持っておけばよいため、スパースベクトルはインデックスのベクタと値のベクタから構成される構造体として実装されます。


スパースベクトルで文書データを表現することにより、劇的に省メモリになり計算時間も短縮されます。一方で、機械学習ライブラリ側がスパースベクトルに対応している必要があります。


==== TF-IDF


単純な単語の出現回数ではなく、単語の重要度の指標を使ってデータを表現することでより精度が向上します。そのような指標の1つとして、@<b>{TF-IDF}があります。



TF-IDFは" Term Frequency - Inverse Document Frequency "の略で、大雑把に言うと、「ある文書中に出現する単語の頻度」を「文書集合全体で出現する単語の頻度」で割ったものです。これは、ある文書中で相対的に頻繁に出てくる単語は重要度が高いと考えられるが、どの文書にも共通して現れるような一般的な単語であるなら、その重要度は差し引いて考えなければならないということを意味しています。


TF-IDFでも文書中に出現していない単語に対する値は0となるので、やはりスパースベクトルになります。


==== テキストデータからデータセットを作る


テキストデータを形態素解析し、TF-IDFで単語の重要度を計算し、スパースベクトルのリストとして返す一連の処理を行うライブラリとして@<tt>{cl-docclass}があります。これはRoswellからインストールすることができます。


//cmd{
$ ros install masatoi/cl-docclass
//}


以下のコードで、先程ダウンロードしたLivedoorニュースコーパスのテキストデータから、機械学習ライブラリへの入力となるリスト@<tt>{*text-sparse-vectors*}が作られます。


//emlist{
;; cl-docclassをロード
(ql:quickload :cl-docclass)

(defparameter *livedoor-data-dir* #P"~/datasets/livedoor/text/")

(defparameter *news-site-names*
  '("kaden-channel" "peachy" "sports-watch" "dokujo-tsushin" "livedoor-homme"
    "topic-news" "it-life-hack" "movie-enter" "smax"))

(defparameter *livedoor-data*
  (mapcar (lambda (p)
            (uiop:directory-files
             (merge-pathnames (concatenate 'string p "/") *livedoor-data-dir*)))
          *news-site-names*))

;; テキストファイルのパスのリスト
(defparameter *livedoor-data-files*
  (alexandria:flatten *livedoor-data*))

;; 単語の文字列をキー、単語の通しインデックスと出現回数のドット対をバリューとするハッシュテーブル
(defparameter *word-hash* (make-hash-table :test 'equal))

;; *word-hash*に値を設定する(数秒かかる)
(dolist (file *livedoor-data-files*)
  (docclass:add-words-to-hash-from-file! file *word-hash*))

;; 頻度の小さい単語を除いてハッシュテーブルを作り直す
(setf *word-hash* (docclass:remove-infrequent-words *word-hash* 10))

;; テキストファイルのリストと*word-hash*から疎ベクトルのリストを作る(十数秒かかる)
(defparameter *text-sparse-vectors*
  (docclass:make-tf-idf-list-from-files *livedoor-data-files* *word-hash*))
//}


@<tt>{*text-sparse-vectors*}はスパースベクトルのリストで、その長さは文書の数だけあります(7367個)。また、総単語数は@<tt>{*word-hash*}から、18372個であることが分かります。しかしながら、以下のように1つの文書に対応するスパースベクトルを表示してみると、実際にはその文書には114個の単語しか出現していないことが分かります。


//emlist{
(length *text-sparse-vectors*) ; => 7367 (文書数)
(hash-table-count *word-hash*) ; => 18372 (単語数)

(car *text-sparse-vectors*)
;; #S(CL-ONLINE-LEARNING.VECTOR::SPARSE-VECTOR
;;    :LENGTH 114
;;    :INDEX-VECTOR #(0 12 13 ...)
;;    :VALUE-VECTOR #(0.021566820323196d0 0.02112624939291d0 0.027720820546932d0 ...))
//}


次に、教師信号のリストを作る必要があります。つまり、各文書がどのクラスに属するかを表すクラスラベルを貼ります。クラスラベルは0以上の整数である必要があるので、テキストファイルが入っている各ディレクトリに番号を振ったものを使います。

//emlist{
(defparameter *class-labels*
  (alexandria:flatten
   (loop for class-id from 0 to (1- (length *livedoor-data*))
         for dir-length in (mapcar #'length *livedoor-data*)
         collect (make-list dir-length :initial-element class-id))))
//}


データセットはクラスラベルとスパースベクトルのドット対からなるリストとして作ります。このままだと同じクラスのデータが連続して並んでいる状態なので、逐次更新するタイプの線形分類器だと性能が悪化してしまいます。そのためデータセットをシャッフルしておきます。


//emlist{
(defparameter *dataset*
  (alexandria:shuffle (mapcar #'cons *class-labels* *text-sparse-vectors*)))
//}


最後に、データセットを訓練用のデータとテスト用のデータに分けます。データセットのうち後半1000個をテストデータとして使い、残りを訓練データとして使います。

//emlist{
(defparameter *train-length* (- (length *text-sparse-vectors*) 1000))
(defparameter *train-set* (subseq *dataset* 0 *train-length*))
(defparameter *test-set* (nthcdr *train-length* *dataset*))
//}


以降では、データからの学習には訓練データを使い、予測性能の評価にはテストデータを使います。


=== 教師あり学習、線形分類器: cl-online-learning


ここまででデータセットの準備ができたので、次は実際に学習していきます。学習に用いる線形分類器にはcl-online-learningを使います。これはQuicklispに登録されているので以下のようにインストールすることができます。

//emlist{
(ql:quickload :cl-online-learning)
//}


cl-online-learningにはいくつか学習アルゴリズムが実装されていますが、ここではAROW (Adaptive Regularization of Weight Vectors)というアルゴリズムを使うことにします。AROWは予測性能がよく、収束も速く、頑健な学習ができるオンラインアルゴリズム@<fn>{online-algorithm}で、メタパラメータが1つしかない上にそのパラメータに対しても反応が穏やかなので扱いやすいアルゴリズムです。

//footnote[online-algorithm][データを逐次処理するアルゴリズムのこと。リアルタイム処理に組込みやすいなどのメリットがある。]

==== マルチクラス分類について


ここでは文書を9クラスに分類しようとしているので、マルチクラス分類というタスクになります。
cl-online-learningで実装されているのは基本的には2値分類のアルゴリズムですが、2値分類器をいくつか組み合わせることでマルチクラス分類ができます。

代表的な2値分類器の組合せ方には@<tt>{1-vs-1}と@<tt>{1-vs-Rest}があります。@<tt>{1-vs-1}の方が一般に精度が良いですが、クラス数の2乗に比例する計算コストがかかります。一方で@<tt>{1-vs-Rest}はクラス数に比例する計算コストで済みます。ここでは@<tt>{1-vs-Rest}を用いることにします。


==== 学習器のオブジェクトを作る


学習器を作るには、@<tt>{clol:make-one-vs-rest}関数を使います。この関数には入力データの次元数(スパースベクトルの本来のサイズ)と、クラス数、学習アルゴリズムの種類を表すシンボル、学習アルゴリズムのメタパラメータを与えます。

AROWの場合、メタパラメータとして1つの正則化パラメータを与えます。これは正の実数で、小さい値であるほど収束が速くなりますが、小さすぎると最終的な予測性能が悪化します。


//emlist{
(defparameter *learner* (clol:make-one-vs-rest 18372 9 'sparse-arow 0.1d0))
//}

==== 訓練とテスト


次に、訓練データセットを使って学習器を訓練し、さらにテストデータセット@<fn>{test}を使って未知のデータに対する予測性能を評価します。

//footnote[test][ここでいうテストとは未知データに対する予測性能を評価するという機械学習の用語で、ソフトウェアテストとは区別する必要があります。]


訓練を実行するには、@<code>{clol:train}関数に学習器オブジェクトと訓練データセットを与えます。これにより、@<code>{*learner*}内のパラメータが破壊的に変更されます。

cl-online-learningは計算のボトルネックになる部分で型宣言などの最適化がなされており、学習スピードは非常に速い部類に入ります。


//emlist{
(time (clol:train *learner* *train-set*))
;; Evaluation took:
;;   0.073 seconds of real time
;;   0.072121 seconds of total run time (0.072117 user, 0.000004 system)
;;   98.63% CPU
;;   143,653,754 processor cycles
;;   425,984 bytes consed
//}


テストには@<tt>{clol:test}を使います。実行結果を見ると、訓練データに対する予測の正答率は100%近くになるものの、未知のデータであるテストデータに対しては95%程度であることが分かります。


//emlist{
(clol:test *learner* *train-set*)
;; Accuracy: 99.79582%, Correct: 6354, Total: 6367

(clol:test *learner* *test-set*)
;; Accuracy: 95.700005%, Correct: 957, Total: 1000
//}

実際には@<code>{clol:train}は訓練データセットを一周するだけで、収束するまでには何回か@<code>{clol:train}を実行する必要があるかもしれません。収束したかどうかは訓練サイクルの各所でテストデータに対する予測性能を見て判断します。

==== 個々のデータの予測


個々のデータに対するクラスラベルを予測するには、@<tt>{clol:one-vs-rest-predict}を使います。
例えば、テストデータの先頭にあるデータに対するクラスラベルは以下のように予測することができます。これを見ると、正解のクラスラベルを正しく予測できていることが分かります。


//emlist{
;; 正解
(car (car *test-set*)) ; => 2

;; 予測値
(clol:one-vs-rest-predict *learner* (cdr (car *test-set*))) ; => 2

//}

== まとめ


この章の前半では、Webスクレイピングの基本的な方法を説明しました。

 * DexadorによってHTMLデータを取得する
 * PlumpによってHTMLデータをパースし、オブジェクトの木構造(DOMツリー)を作る
 * CLSSによって特定のノードを探す
 * ログインが必要なサイトへのアクセス


章の後半では、集めたテキストデータを教師あり学習で分類する方法を紹介しました。

 * cl-igoでテキストデータを形態素解析する
 * cl-docclassでテキストデータからの特徴抽出と前処理をする
 * cl-online-learningでマルチクラス分類をする
