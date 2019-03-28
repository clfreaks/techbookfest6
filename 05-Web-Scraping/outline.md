# Webスクレイピングと文書分類
## この章でやること
インターネットを流れるトラフィックは量的な意味では動画データが過半数を占めるようになりつつあるが、依然としてテキストデータの重要性は変わらない。テキストデータは日々ものすごいスピードで増え続けており、一人の人間の目で追い続けるのはもはや不可能である。また、それらのテキストデータは大抵の場合構造化されておらず、そのままでは使えないことが多い。そこでデータ分析が必要になる。テキストデータをデータ分析し、意味ある構造を取り出す行為をテキストマイニングと呼ぶ。

本章では、Common Lispのライブラリを用いて、インターネットをクローリングし、必要な情報を収集した上で構造化するためのいくつかの方法を紹介する。

## Webスクレイピング
### Webスクレイピングに必要なライブラリ
Webスクレイピングとはウェブサイトから必要な情報を取り出す行為のことをいう。
Webスクレイピングでは、(1) Webからデータを取得し、(2) これを解析し、扱いやすいデータ構造を作り、(3) そこから必要な情報を探索するというプロセスを踏む。
Common Lispにはこの各段階に対応するライブラリがある。以下ではそのインストール方法と基本的な使用例を解説する。

### HTTPクライアント: Dexador
Webサイトからデータを取得するためにはHTTPクライアントのDexador(デキサドル)を使用する。Common Lispには古くからDrakmaというHTTPクライアントがあるが、Dexadorの方が(特に同じホストに複数回アクセスする場合において)速く動作する。
#### インストール
インストールはQuicklispから行う。

```
(ql:quickload :dexador)
```

####  使い方
Dexadorの最も基本的かつ重要な関数は`dex:get`で、GETメソッドで指定のURLからデータを取得する。次のコードは`http://lisp.org/`にアクセスし、そこから取得したHTMLデータを文字列として`*html-source*`に設定する。

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
このうち最初のデータ本体は次節で扱うHTMLパーサへの入力として用いる。また、HTTPステータスコードはデータの取得に成功したか失敗したか、さらにはその理由を調べるのに役に立つ。

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

`traverse`にはDOMツリーの各ノードに適用される関数を渡す。キーワードパラメータ`:test`に述語関数を渡すと、それを満足するノードに対してのみ関数が適用される。ここでは述語関数`plump:text-node-p`を渡しているので、テキストデータを持つノードに対してのみ関数が適用され、そのノードの持つ文字列をリストに`push`していっている。最後に`nreverse`で順序を反転させ、`concatenate`で連結して返すという関数だ。

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
(plump:traverse *root-node*
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

Chromeなどの最近のWebブラウザにはインスペクタが搭載されており、Webページ上の調べたいところを右クリックして「検証」や「Inspect」などと表記されている項目を選べば、HTMLソース中の対応する部分がハイライトされる。

![Chrome上でインスペクタを起動した画面](images/05-clojure.png)

このサイトをブラウザのインスペクタで調べると、内容のまとまりで`w-section`というクラスが付いていることが分かる。CLSSを使えば、このクラス名でDOMツリーを検索することができる。なお、CSSと同様に、クラスの場合はクラス名の前に`.`を付け、IDの場合はID名の前に`#`を付ける。また、タグとの組み合わせで指定することもできる。

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

Chromeにはインスペクタの他にもHTTPリクエストとそのレスポンスを記録する機能などもあり、それらはDevToolsという画面にまとまっている。DevToolsは右上のメニューから「More tools > Developer tools」を選択するか、キーボードから`Ctrl + Shift + I`を入力することで開くことができる。

### ログインを必要とするサイト
さて、ここまでのところはデータの取得は`dex:get`のみで事足りていたが、ログインを必要とするサイトではフォームからログイン情報を送信し、さらにその後のセッション管理が必要になるため、やや複雑になる。

Dexadorには`dex:get`の他にも、`dex:post`や`dex:request`などのリクエスト関数があり、ほとんど同じように使える。これらのリクエスト関数にはCookieオブジェクトを渡すことができ、これを使ってログイン状態を保持できる。
全体の流れとしては、まず空のCookieオブジェクトを生成し、それを`dex:post`に渡してフォームをサーバに送信することでCookieに値が設定される。以降では同じCookieオブジェクトを`dex:get`などに渡すとログインした状態でHTMLデータが取得できるようになる。

ここでは例として、はてなブックマークの「マイホットエントリー」のページを取得することを考える。これははてなIDに連携されたSNSなどの情報を元に、各ユーザに合わせたおすすめ記事を表示するページであり、ログインした状態でないと見られない。したがって、Cookieのない状態で、以下のはてなID`(masatoi`)のマイホットエントリーページを取得してみると、アクセス権限が無いというエラーメッセージが返る。

```
(dex:get "http://b.hatena.ne.jp/masatoi/hotentry")

;; An HTTP request to "http://b.hatena.ne.jp/masatoi/hotentry" returned 403 forbidden.
```

次に、はてなのログインフォームにどのような情報を送るべきかを調べる必要がある。これにはChromeなどを使い実際にログインしてみて、開発ツール(DevTools)を使ってどのようなデータがPOSTされているのかを調べるのがいいだろう。

まず、ブラウザではてなのログインページ(`https://www.hatena.ne.jp/login`)を開き、DevToolsからNetworkタブを開く。この画面にはHTTPリクエストやそれに対するレスポンスが記録される。なお、ログイン成功時には、はてなのトップページへリダイレクトされるので、ページの遷移や再読み込みでログが消えないように「Preserve log」チェックボックスを選択しておく必要がある。
DevToolsのNetworkタブを開いた状態で、Webページ上のログインフォームにログイン情報を入力して送信すると、たくさんのログが表示されるので、ここからログイン情報を送っているPOSTリクエストを探す。左上の検索ウインドウからメソッドを指定してリクエストを検索することができるので、`method:POST`と入力するとPOSTリクエストが検索できる。

![DevTools上でNetworkタブを選択しPOSTメソッドのHTTPリクエストを表示した画面](images/05-hatena-login-network.png)

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

ログインと同様に、Webサイトからログアウトするときもログアウト用のURLにPOSTリクエストを送るだけである。以下のように、ログインして一連の処理を行い、最後に必ずログアウトするようなマクロ`with-hatena`を考えることができる。

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

このマクロ内で生成されるCookieオブジェクトは局所変数に束縛され、本体部分で使用することができる。`with-hatena`を使うことで、ログイン/ログアウト処理を意識することなくログインが必要なURLにアクセスすることができる。

```
(with-hatena (cookie-jar) (:id "masatoi" :password "hogehoge")
  (dex:get "http://b.hatena.ne.jp/masatoi/hotentry" :cookie-jar cookie-jar))
```

実際のところ、ログインのためにJavaScriptを必要とするサイトや、ユーザIDとパスワード以外にCSRFトークンなどを必要とするサイトも多く、サイト毎にログイン方法は異なる。しかし大枠としては、ここで解説したような流れでログインできるだろう。

## 文書分類
この節では、Webスクレイピングで得られた大量のテキストデータから、機械学習アルゴリズムを使って、どのように構造を取り出すかについて解説する。
ここでは特に、文書分類について解説する。

文書分類は教師あり学習の問題であり、文書に対してあらかじめラベル(記事カテゴリやスパムか否かなど)が付いているデータで学習し、未知の文書に対してその文書がどのラベルかを推定する。例えばメールソフトのスパムフィルタや自動タグ付けがこれに当たる。

Common Lispには機械学習ライブラリの数こそ少ないが、外部ライブラリに頼らなくても高速な数値計算ができるので、こういったデータ分析には向いている。

### 文書分類に必要なライブラリ
日本語の文書を分析するためには、まず文書から単語を切り出す必要がある。
分かち書きされていない文から語句を切り出し、それぞれに品詞を付けることを形態素解析といい、有名なものではMeCabなどのオープンソースの形態素解析エンジンが公開されている。Common LispにはMeCabの辞書を利用し、推論自体はピュアCommon Lispで行うcl-igoというライブラリがある。

文書分類には線形分類器を利用する。Common Lispで実装された高速な線形分類器としてはcl-online-learningがある。cl-online-learningは割と最近のアルゴリズムもカバーしており、スパースなデータにも対応している。スパース(疎)なデータとは入力値の大部分が0であるデータのことで、文書をスパースデータとして表現する方法についても後に解説する。

### 形態素解析器: cl-igo

[cl-igo](http://igo.osdn.jp/cl-igo.html)はCommon Lispから使える形態素解析器で、MeCabの辞書を変換したバイナリ辞書を使う。辞書の変換にはJavaを使用するが、その後の推論部分はCommon Lispのみで実装されている。

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
最後に、LispのREPLなどで`ql:quickload`でcl-igoをロードし、`igo:load-tagger`で先程作ったバイナリ辞書のディレクトリを指定して読み込む。
以降は`igo:parse`関数に日本語の文を与えればそれを形態素解析した結果のリストが返る。

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
#### Livedoorニュースコーパス
ここでは説明のために、クリエイティブコモンズライセンスで公開されている[Livedoorニュースコーパス](https://www.rondhuit.com/download.html#ldcc)を使用する。

このデータセットは9つのニュースサイトから合計7367個の記事を集めたものであり、出典ごとにディレクトリに分けて、1記事1ファイルのテキストファイルとして収録されている。以降では、ここでダウンロードしたファイルを`~/datasets/livedoor`に展開したものとして話を進める。

#### 文書データのスパースベクトルとしての表現
機械学習アルゴリズムを使ってデータから学習するには、まずデータの特徴量を適切に定める必要がある。

文書集合を構成する各文書は、1つの実数ベクトルとして表現できる。表現方法としては色々なものが考えられるが、文書分類の場合は文書中に出現する単語の頻度に注目した表現方法がよく使われる。具体的には、文書集合全体に現れる全単語数分の長さを持つベクトルを用意し、各単語に対応する位置に出現した回数を記録する。

文書集合全体に現れる単語数は3万語以上あるが、1つの文書の中に現れる単語数は高々2、3百個でしかない。そのため、文書に対応するベクトルはほとんどが0の疎なベクトル(スパースベクトル)になる。このような場合、0でない要素のインデックスと値だけを持っておけばよいため、スパースベクトルはインデックスのベクタと値のベクタから構成される構造体として実装される。

スパースベクトルで文書データを表現することにより、劇的に省メモリになり計算時間も短縮されるが、機械学習ライブラリが対応している必要がある。

#### TF-IDF
単純な単語の出現回数ではなく、単語の重要度の指標を使ってデータを表現することでより精度が向上する。そのような指標の1つにTF-IDFがある。

TF-IDFは" Term Frequency - Inverse Document Frequency "の略で、「ある文書中に出現する単語の頻度」を「文書集合全体で出現する単語の頻度」で割ったものである。この意味するところは、ある文書中で相対的に頻繁に出てくる単語は重要度が高いと考えられるが、どの文書にも共通して現れるような一般的な単語であるなら、その重要度は差し引いて考える、ということである。

TF-IDFでも文書中に出現していない単語に対する値は0となるので、やはりスパースベクトルになる。

#### テキストデータからデータセットを作る

テキストデータを形態素解析し、TF-IDFで単語の重要度を計算し、スパースベクトルのリストとして返す一連の処理を行うライブラリとして`cl-docclass`がある。これはRoswellからインストールできる。

```
ros install masatoi/cl-docclass
```

以下のコードで、先程ダウンロードしたLivedoorニュースコーパスのテキストデータから、機械学習ライブラリへの入力となるリスト`*text-sparse-vectors*`が作られる。

```
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
```

`*text-sparse-vectors*`はスパースベクトルのリストで、その長さは文書数だけあるので、7367個ある。総単語数は`*word-hash*`から、18372個であることが分かる。1つの文書に対応するスパースベクトルを表示してみると、実際には114個の単語しか出現していないことが分かる。

```
(length *text-sparse-vectors*) ; => 7367 (文書数)
(hash-table-count *word-hash*) ; => 18372 (単語数)

(car *text-sparse-vectors*)
;; #S(CL-ONLINE-LEARNING.VECTOR::SPARSE-VECTOR
;;    :LENGTH 114
;;    :INDEX-VECTOR #(0 12 13 ...)
;;    :VALUE-VECTOR #(0.021566820323196d0 0.02112624939291d0 0.027720820546932d0 ...))
```

次に、教師信号のリストを作る必要がある。つまり、各文書がどのクラスに属するかを表すクラスラベルを貼る。これにはテキストファイルが入っているディレクトリに番号を振ったものを使う。

```
(defparameter *class-labels*
  (alexandria:flatten
   (loop for class-id from 0 to (1- (length *livedoor-data*))
         for dir-length in (mapcar #'length *livedoor-data*)
         collect (make-list dir-length :initial-element class-id))))
```

データセットはクラスラベルとスパースベクトルのドット対からなるリストとして作る。このままだと同じクラスのデータが連続して並んでいるので、逐次更新するタイプの線形分類器だと性能が悪化してしまう。そのためデータセットをシャッフルする。

```
(defparameter *dataset*
  (alexandria:shuffle (mapcar #'cons *class-labels* *text-sparse-vectors*)))
```

最後に、データセットを訓練用のデータとテスト用のデータに分ける。

```
(defparameter *train-set* (subseq *dataset* 0 6367)) ; 前半6367個を訓練データとして使う
(defparameter *test-set* (nthcdr 6367 *dataset*)) ; 後半1000個をテストデータとして使う
```

以降では、データからの学習には訓練データを使い、予測性能の評価にはテストデータを使う。

### 教師あり学習、線形分類器: cl-online-learning

ここまででデータセットの準備ができたので、次は実際に学習していく。学習に用いる線形分類器にはcl-online-learningを使う。これはQuicklispに登録されているので以下のようにインストールできる。

```
(ql:quickload :cl-online-learning)
```

cl-online-learningにはいくつか学習アルゴリズムが実装されているが、ここではAROW (Adaptive Regularization of Weight Vectors)というアルゴリズムを使う。AROWは予測性能がよく、収束も速く、頑健な学習ができるオンラインアルゴリズム(データを逐次処理するアルゴリズムのこと。リアルタイム処理に組込みやすいなどのメリットがある)で、メタパラメータが1つしかない上にそのパラメータに対しても過敏に反応しないので扱いやすい。

#### マルチクラス分類について

ここでは文書を9クラスに分類しようとしているので、マルチクラス分類というタスクになる。
cl-online-learningで実装されているのは基本的には2値分類のアルゴリズムだが、2値分類器をいくつか組み合わせることでマルチクラス分類ができる。

代表的な2値分類器の組合せ方には`1-vs-1`と`1-vs-Rest`がある。`1-vs-1`の方が一般に精度が良いが、クラス数の2乗に比例する計算コストがかかる。一方で`1-vs-Rest`はクラス数に比例する計算コストで済む。ここでは`1-vs-Rest`を用いることにする。

#### 学習器のオブジェクトを作る

学習器を作るには、`clol:make-one-vs-rest`関数を使う。この関数には入力データの次元数(スパースベクトルの本来のサイズ)と、クラス数、学習アルゴリズムの種類を表すシンボル、学習アルゴリズムのメタパラメータを与える。

AROWの場合、メタパラメータとして1つの正則化パラメータを与える。これは正の実数で、小さい値であるほど収束が速くなるが、小さすぎると精度が悪化する。

```
(defparameter *learner* (clol:make-one-vs-rest 18372 9 'sparse-arow 0.1d0))
```

#### 訓練とテスト

次に、訓練データセットを使って学習器を訓練し、さらにテストデータセットを使って未知のデータに対する予測性能を評価する。

訓練を実行するには、`clol:train`関数に学習器オブジェクトと訓練データセットを与える。これにより、`*leaner*`内のパラメータが破壊的に変更される。

cl-online-learningは計算のボトルネックになる部分で型宣言などの最適化がなされており、学習スピードは非常に速い。同様の処理をするC++プログラムより速いこともある。

```
(time (clol:train *learner* *train-set*))
;; Evaluation took:
;;   0.073 seconds of real time
;;   0.072121 seconds of total run time (0.072117 user, 0.000004 system)
;;   98.63% CPU
;;   143,653,754 processor cycles
;;   425,984 bytes consed
```

テストには`clol:test`を使う。訓練データに対する予測の正解率は100%近くになるが、未知のデータであるテストデータに対しては95%程度であることが分かる。

```
(clol:test *learner* *train-set*)
;; Accuracy: 99.79582%, Correct: 6354, Total: 6367

(clol:test *learner* *test-set*)
;; Accuracy: 95.700005%, Correct: 957, Total: 1000
```

#### 個々のデータの予測

1つのデータに対するクラスラベルを予測するには、`clol:one-vs-rest-predict`を使う。
例えば、テストデータセットの先頭のデータに対するクラスラベルの予測値は以下のように出すことができ、正解のクラスラベルを正しく予測できていることが分かる。

```
;; 正解
(car (car *test-set*)) ; => 2

;; 予測値
(clol:one-vs-rest-predict *learner* (cdr (car *test-set*))) ; => 2

```

## まとめ
この章の前半では、Webスクレイピングの基本的な方法を説明した。

- DexadorによってHTMLデータを取得する
- PlumpによってHTMLデータをパースし、オブジェクトの木構造(DOMツリー)を作る
- CLSSによって特定のノードを探す
- ログインが必要なサイトへのアクセス

章の後半では、集めたテキストデータを教師あり学習で分類する方法を紹介した。

- cl-igoでテキストデータを形態素解析する
- cl-docclassでテキストデータからの特徴抽出と前処理をする
- cl-online-learningでマルチクラス分類をする
