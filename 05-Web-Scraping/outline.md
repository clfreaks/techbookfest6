# Webスクレイピングと文書分類
## この章でやること
インターネット上を流れるトラフィックは量的な意味では動画データが過半数を占めるようになりつつあるが、依然としてテキストデータの重要性は変わらない。テキストデータは日々ものすごいスピードで増え続けており、一人の人間の目で追い続けるのはもはや不可能である。また、それらのテキストデータは大抵の場合構造化されておらず、そのままでは使えないことが多い。そこでデータ分析が必要になる。テキストデータをデータ分析し、意味ある構造を取り出す行為をテキストマイニングと呼ぶ。

本章では、Common Lispのライブラリを用いて、インターネットをクローリングし、必要な情報を収集した上で構造化するためのいくつかの方法を紹介する。

## Webスクレイピング
### Webスクレイピングに必要なライブラリ
Webスクレイピングとはウェブサイトから必要な情報を取り出す行為のことをいう。
Webスクレイピングでは、(1) Webからデータを取得し、(2) これを解析し扱いやすいデータ構造を作り、(3) そこから必要な情報を探索するというプロセスを踏む。
Common Lispにはこの各段階について対応したライブラリがある。以下ではそのインストール方法と基本的な使用例を解説する。

### HTTPクライアント: Dexador
ウェブサイトからデータを取得するためにはHTTPクライアントのDexador(デキサドル)を使用する。Common Lispには古くからDrakmaというHTTPクライアントがあるが、Dexadorの方が(特に同じホストに複数回アクセスする場合において)速く動作する。
#### インストール
インストールはQuicklispから行う。

```
(ql:quickload :dexador)
```

####  使い方
Dexadorの最も基本的かつ重要な関数は`dex:get`で、GETメソッドで指定のURLからデータを取得する。次のコードは`http://lisp.org/`にアクセスし、そこから取得したHTMLデータを文字列として`*html-source*`に設定している。

```
(defparameter *html-source* (dex:get "http://lisp.org/"))
```
`get`関数は(1) 本体データの文字列、(2) HTTPステータスコード、(3) レスポンスヘッダーのハッシュテーブル、(4) URI構造体、(5) 読み出し元ソケットのストリームの5つを多値で返す。次は上で実行した`get`関数の全ての返り値を列挙したものである。

```
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
200
#<HASH-TABLE :TEST EQUAL :COUNT 10>
#<QURI.URI.HTTP:URI-HTTPS https://lisp.org/>
#<CL+SSL::SSL-STREAM for #<FD-STREAM>>
```
このうち特に重要なのは最初の2つで、データ本体は次節で扱うHTMLパーサへの入力として用い、HTTPステータスコードはデータの取得に成功したか失敗したか、さらにはその理由を調べるのに役に立つ。

### XML/HTMLパーサ: Plump
ここで紹介するPlumpと次節で紹介するCLSSは同じ作者のプロダクトで、セットで使われる。これらはDexadorと同様にQuicklispからインストールできる。以下のように、`ql:quickload`には複数のパッケージ名をリストとして与えることもできる。

```
(ql:quickload '(:plump :clss))
```

PlumpはXML/HTMLパーサであり、XML/HTMLデータを文字列として受け取り、CLOSオブジェクトのノードから構成される木構造(DOMツリー)を生成し、そのルートノードのオブジェクトを返す。

例えば以下のようにして、前述のDexadorの`get`の結果をパースすると、ルートノードのオブジェクトが返る。


```
(defparameter *root-node* (plump:parse *html-source*))

;; *root-node* => #<PLUMP-DOM:ROOT>
```

各ノードの子ノードのベクタは`plump:children`で、親ノードは`plump:parent`でそれぞれ得られる。


```
(plump:children *root-node*)

;; => #(#<PLUMP-DOM:ELEMENT HTML> #<PLUMP-DOM:TEXT-NODE>)

(plump:parent (aref (plump:children *root-node*) 0))

;; => #<PLUMP-DOM:ROOT>
```

また、DOMツリーを走査し、各ノードに対して関数を適用する高階関数`plump:traverse`が用意されている。
例えば、テキストノードから文字列を取り出して連結して返す関数は以下のようになる。

```
(defun concat-node-text (node)
  (let ((text-list nil))
    (plump:traverse node
                    (lambda (node) (push (plump:text node) text-list))
                    :test #'plump:text-node-p)
    (apply #'concatenate 'string (nreverse text-list))))

(concat-node-text *root-node*)
```

`traverse`には各ノードについて適用される関数を渡す。キーワードパラメータ`:test`に述語関数を渡すと、それを満足するノードに対してのみ第二引数の関数が適用される。ここでは述語関数`plump:text-node-p`を渡しているので、テキストデータを持つノードに対して関数が適用され、そのノードの持つ文字列をリストに`push`していっている。最後に`nreverse`で順序を反転させ、`concatenate`で連結して返すという関数だ。

この関数の評価結果は以下のようになるだろう。

```
"John McCarthy, 1927-2011
John McCarthy
1927-2011
John McCarthy's Home Page
Obituary"
```

同様にして、取得したDOMツリーに対して変更を加えることもできる。例えばテキストノードの文字列を全て大文字に変更するには、`traverse`でDOMツリーを走査しながら、`text-node-p`を満足するノードオブジェクトのスロットに`setf`で新しい値を設定すればよい。

```
(plump:traverse root
                (lambda (node)
                  (setf (plump:text node)
                        (string-upcase (plump:text node))))
                :test #'plump:text-node-p)

(concat-node-text *root-node*)
```

```
"JOHN MCCARTHY, 1927-2011
JOHN MCCARTHY
1927-2011
JOHN MCCARTHY'S HOME PAGE
OBITUARY"
```

以上のように、Plumpを使えばHTMLからDOMツリーを生成し、そこから情報を取り出したり変更を加えることができる。

### CSSセレクタ: CLSS
次はDOMツリーから欲しい情報を取り出す方法を紹介する。
先ほどPlumpと一緒にインストールしたCLSSはCSSセレクタと呼ばれるもので、XML/HTMLのタグやCSSクラス、IDなどを使ってDOMツリーからノードを検索することができる。

#### タグ名による検索

例えばIMGタグでルートノード以下を検索すると、Plumpのノードのベクタが得られる。その最初の要素を変数`*img-node*`に持っておく。

```
(defparameter *img-nodes* (clss:select "img" *root-node*))

;; *img-nodes* => #(#<PLUMP-DOM:ELEMENT img {1008AEE273}>)

(defparameter *img-node* (aref *img-nodes* 0))
(describe *img-node*)
```

このノードを`describe`してみると、子ノードを持っておらず、タグ名はIMGで、属性の情報をハッシュテーブルとして持っていることが分かる。

```
Slots with :INSTANCE allocation:
  %PARENT                        = #<PLUMP-DOM:ELEMENT BODY {100D7D8EC3}>
  %CHILDREN                      = #()
  %TAG-NAME                      = "img"
  %ATTRIBUTES                    = #<HASH-TABLE :TEST EQUALP :COUNT 2 {100D7DA2F3}>
```

なお、`plump:attributes`でノードオブジェクトから属性のハッシュテーブルを取得でき、各属性値には`plump:attribute`でアクセスできる。このハッシュテーブルの中身を表示してみると、属性値としてsrcとaltを持っていることが分かる。

```
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
```

ここまでに説明したことを使えば、例えば全てのAタグを検索し、リンク先を列挙するようなこともできる。


```
(loop for a-node across (clss:select "a" *root-node*)
      collect (plump:attribute a-node "href"))

;; ("http://www-formal.stanford.edu/jmc/"
;;  "http://news.stanford.edu/news/2011/october/john-mccarthy-obit-102511.html")
```

#### CSSクラス、IDによる検索
次に、CSSのクラスやIDをキーとして、DOMツリーからノードを検索する例を紹介する。まず例として、Clojureの公式サイトを取得する。

```
(defparameter *clojure-root-node*
  (plump:parse (dex:get "https://clojure.org/")))
```

Chromeなどの最近のWebブラウザにはインスペクタが搭載されており、Webページ上の調べたいところを右クリックして「検証」や「Inspect」などと表記されている項目を選べば、HTMLソース中の対応する部分がハイライトされる。このサイトをブラウザのインスペクタで調べると、内容のまとまりで`w-section`というクラスが付いていることが分かる。例えば、このクラス名でDOMツリーを検索することができる。なお、CSSと同様に、クラスの場合はクラス名の前に`.`を付け、IDの場合はID名の前に`#`を付ける。また、タグとの組み合わせで指定することもできる。

![Chrome上でインスペクタを起動した画面](images/05-clojure.png)

```
(clss:select ".w-section" *clojure-root-node*)

;; #(#<PLUMP-DOM:ELEMENT div {1017135813}> #<PLUMP-DOM:ELEMENT div {1017141093}>
;;   #<PLUMP-DOM:ELEMENT div {101714B7E3}> #<PLUMP-DOM:ELEMENT div {101716C8F3}>
;;   #<PLUMP-DOM:ELEMENT div {1017186913}> #<PLUMP-DOM:ELEMENT div {10171A9623}>
;;   #<PLUMP-DOM:ELEMENT div {10171CFF53}> #<PLUMP-DOM:ELEMENT div {10171DFC83}>)

(clss:select "div.clj-intro-message" *clojure-root-node*)

;; #(#<PLUMP-DOM:ELEMENT div {101A9D53F3}>)
```

### スクレイピングの実際
ここまでに説明したことを用いれば、様々なサイトから情報を取り出すことができる。`dex:get`でサイトのURLからHTMLを文字列として取得し、`plump:parse`でCLOSオブジェクトのノードから成るDOMツリーを作り、`clss:select`で所望のノードを取り出す。その後は`concat-node-text`や`plump:attribute`などで必要な値を取り出せばよい。

Chromeの場合、インスペクタのHTMLソースから、クラスやID、タグといったものを調べられる。また、HTMLソースの要素上で右クリックして「Copy > Copy selector」を選択することで、その要素に対応するCSSセレクタをクリップボードにコピーできる。これをそのまま`clss:select`に渡せば該当部分に対応するノードを得られる。

Chromeにはインスペクタの他にもHTTPのリクエストやレスポンスを記録する機能などもあり、それらはDevToolsという画面にまとまっている。DevToolsは右上のメニューから「More tools > Developer tools」を選択するか、キーボードから`Ctrl + Shift + I`を入力することで開くことができる。

### ログインを必要とするサイト
さて、ここまでのところはデータの取得は`dex:get`のみで事足りていたが、ログインを必要とするサイトではフォームからログイン情報を送信し、さらにその後のセッション管理が必要になるため、やや複雑になる。

Dexadorのリクエスト関数にはCookieオブジェクトを渡すことができ、これを使ってログイン状態を保持できる。全体の流れとしては、まず空のCookieオブジェクトを生成し、それを`dex:post`に渡してフォームをサーバに送信することでCookieに値が設定される。以降では同じCookieオブジェクトを`dex:get`などに渡すとログインした状態でHTMLデータが取得できるようになる。

ここでは例として、はてなブックマークの「マイホットエントリー」のページを取得することを考える。これははてなIDに連携されたSNSなどの情報を元に、各ユーザに合わせたおすすめ記事を表示するページであり、ログインした状態でないと見られない。したがって、Cookieのない状態で、以下のはてなID`(masatoi`)のマイホットエントリーページを取得してみると、アクセス権限が無いというエラーメッセージが返る。

```
(dex:get "http://b.hatena.ne.jp/masatoi/hotentry")

;; An HTTP request to "http://b.hatena.ne.jp/masatoi/hotentry" returned 403 forbidden.
```

次に、はてなのログインフォームにどのような情報を送るべきかを調べる必要がある。これにはChromeなどを使い実際にログインしてみて、開発ツール(DevTools)を使ってどのようなデータがPOSTされているのかを調べるのがいいだろう。

まず、ブラウザではてなのログインページ(`https://www.hatena.ne.jp/login`)を開き、DevToolsからNetworkタブを開く。この画面にはHTTPリクエストやそれに対するレスポンスが記録される。なお、ログイン成功時には、はてなのトップページへリダイレクトされるので、ページの遷移や再読み込みでログが消えないように「Preserve log」チェックボックスを選択しておく必要がある。
DevToolsのNetworkタブを開いた状態で、Webページ上のログインフォームにログイン情報を入力して送信すると、たくさんのログが表示されるので、ここからログイン情報を送っているPOSTリクエストを探す。左上の検索ウインドウからメソッドを指定してリクエストを検索することができるので、`method:POST`と入力するとPOSTリクエストが検索できる。

![DevTools上でNetworkタブを選択し、POSTメソッドのHTTPリクエストを表示した画面](images/05-hatena-login-network.png)

そうして出てきたのが上図の画面である。ここにはPOSTメソッドで送信しているフォームデータが表示されている(パスワードは平文で表示されているので隠してある)。DexadorでPOSTリクエストを送るときはこのフォームデータの形式に従えばよい。

次のコードでは、まず`cl-cookie:make-cookie-jar`でCookieオブジェクトを生成し、それを`dex:post`に渡してユーザ名とパスワードとともに送る。そうすると`*cookie-jar*`に値が設定されるので、それを`dex:get`に渡して先程のマイホットエントリーのページを取得すると、今度はHTMLデータが得られるようになっている。

```
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
```

ログインと同様に、ログアウトするときもログアウト用のURLにPOSTリクエストを送るだけである。ログインして一連の処理を行い、最後に必ずログアウトするようなマクロ`with-hatena`を考えることができる。

```
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
```

このマクロでは内部で生成されるCookieは局所変数に束縛され、このマクロ内でのみ使用することができる。`with-hatena`を使うことで、ログイン/ログアウト処理を意識することなくログインが必要なURLにアクセスすることができる。

```
(with-hatena (cookie-jar) (:id "masatoi" :password "hogehoge")
  (dex:get "http://b.hatena.ne.jp/masatoi/hotentry" :cookie-jar cookie-jar))
```

実際のところ、ログインのためにJavascriptを必要とするサイトや、ユーザIDとパスワード以外にCSRFトークンなどを必要とするサイトも多く、サイト毎にログイン方法は異なる。しかし大枠としては、ここで解説したような流れでログインできるだろう。

## 文書分類 / 文書クラスタリング
この節では、Webスクレイピングで得られた大量のテキストデータからどのように構造を取り出すかについて解説する。
ここでは特に、文書分類と文書クラスタリングについて解説する。

文書分類は教師あり学習の問題であり、文書に対してあらかじめラベル(記事カテゴリやスパムか否かなど)が付いているデータで学習し、未知の文書に対してその文書がどのラベルかを推定する。例えばメールソフトのスパムフィルタや自動タグ付けがこれに当たる。

これに対して文書クラスタリングは教師なし学習であり、学習データにラベルが付いていない。この場合、文書のまとまり(クラスタ)の数だけが与えられて、それぞれの文書の内容からどのクラスタに近いかを判定する。そうしてできたクラスタは近しい内容の文書のまとまりとして解釈できるが、それぞれのクラスタが実際に何を意味しているのかはクラスタと単語の関係を調べてみてはじめて分かる。

Common Lispには機械学習ライブラリの数こそ少ないが、外部ライブラリに頼らなくても高速な数値計算ができるので、こういったデータ分析には向いている。

### 文書分類 / 文書クラスタリングに必要なライブラリ
日本語の文書を分析するためには、まず文書から単語を切り出す必要がある。
分かち書きされていない文から語句を切り出し、それぞれに品詞を付けることを形態素解析といい、有名なものではMeCabなどのオープンソースの形態素解析エンジンが公開されている。Common LispにはMeCabの辞書を利用し、推論自体はピュアCommon Lispで行うcl-igoというライブラリがある。

文書分類には線形分類器を利用する。Common Lispで実装された高速な線形分類器としてはcl-online-learningがある。cl-online-learningは割と最近のアルゴリズムもカバーしており、スパースなデータにも対応している。スパース(疎)なデータとは入力値の大部分が0であるデータのことで、文書をスパースデータとして表現する方法についても後に解説する。

文書クラスタリングにはテンソル分解という手法を利用する。Common Lispにはcl-tensor-decompositionというスパーステンソルに対応したテンソル分解の実装があるのでこれを利用する。

### 形態素解析器: cl-igo

[cl-igo](http://igo.osdn.jp/cl-igo.html)はCommon Lispから使える形態素解析器で、MeCabの辞書を変換したバイナリ辞書を使う。

#### cl-igoのインストール

RoswellからインストールできるようにGitHubにミラーを作ったので、以下のようにしてインストールすることができる。

```
ros install masatoi/charseq masatoi/cl-igo
```

#### cl-igoの辞書を用意する

次にバイナリ辞書を作る。バイナリ辞書のビルドにはJava版のIgoが必要になる。Javaのランタイムは既にインストールされているものとする。
以下のURLからMeCabの辞書データ(`mecab-ipadic-2.7.0-20070801.tgz`)と、Igoのjarファイル(`igo-0.4.5.jar`)をダウンロードする。

- `https://sourceforge.net/projects/mecab/files/mecab-ipadic/2.7.0-20070801/`
- `https://osdn.net/projects/igo/downloads/55029/igo-0.4.5.jar/`

ここでダウンロードした2つのファイルと同じディレクトリで次のコマンドを実行すると、`~/igo/ipadic`というディレクトリに辞書ができる。

```
tar xzvf mecab-ipadic-2.7.0-20070801.tar.gz

java -cp ./igo-0.4.5.jar net.reduls.igo.bin.BuildDic ~/igo/ipadic \
mecab-ipadic-2.7.0-20070801 EUC-JP
```

#### cl-igoをロードし、形態素解析を実行する
最後に、LispのREPLなどで`ql:quickload`でcl-igoをロードし、`igo:load-tagger`で先程作ったバイナリ辞書のディレクトリを指定して読み込む。ここまでで準備は完了である。以降は`igo:parse`関数に日本語の文を与えればそれを形態素解析した結果のリストが返る。

```
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
```

### データセットの作成
#### TF-IDF
#### スパースなデータの取り扱い
### 教師あり学習、線形分類器: cl-online-learning
### 教師なし学習、テンソル分解: cl-tensor-decomposition

## まとめ
この章では、Webスクレイピングの基本的な方法と、集めたテキストデータを分類したりクラスタリングする方法を紹介した。
