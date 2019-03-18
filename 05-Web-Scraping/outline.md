# Webスクレイピングと文書分類
## この章でやること
インターネット上を流れるトラフィックは量的な意味では動画データが過半数を占めるようになりつつあるが、依然としてテキストデータの重要性は変わらない。テキストデータは日々ものすごいスピードで増え続けており、一人の人間の目で追い続けるのはもはや不可能である。また、それらのテキストデータは大抵の場合構造化されておらず、そのままでは使えないことが多い。そこでデータ分析が必要になる。テキストデータをデータ分析し、意味ある構造を取り出す行為をテキストマイニングと呼ぶ。

本章では、Common Lispのライブラリを用いて、インターネットをクローリングし、必要な情報を収集した上で構造化するためのいくつかの方法を紹介する。

## Webスクレイピング
### Webスクレイピングに必要なライブラリ
Webスクレイピングとはウェブサイトから必要な情報を取り出す行為のことをいう。
Webスクレイピングでは、(1) Webからデータを取得し、(2) これを解析しDOM木構造を作り、(3) そこから必要な情報を探索するというプロセスを踏む。
Common Lispにはこの各段階について対応したライブラリがあるので、以下ではそのインストール方法と基本的な使用例を解説する。

### HTTPクライアント: Dexador
ウェブサイトからデータを取得するためにはHTTPクライアントのDexador(デキサドル)を使用する。Common Lispには古くからDrakmaというHTTPクライアントがあるが、Dexadorの方が(特に同じホストに複数回アクセスする場合において)速く動作する。
#### インストール
インストールはQuicklispから行なえる。
```lisp
(ql:quickload :dexador)
```

####  使い方
Dexadorの最も基本的かつ重要な関数は`dex:get`で、GETメソッドで指定のURLからデータを取得する。

```
(defparameter *html-source* (dex:get "http://lisp.org/"))
```
get関数は(1) 本体データの文字列、(2) ステータスコード、(3) レスポンスヘッダーのハッシュテーブル、(4) URI構造体、(5) 読み出し元ソケットのストリームの5つを多値で返す。

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
<a href=\"http://news.stanford.edu/news/2011/october/john-mccarthy-obit-102511.html\">Obituary</a>
</BODY>
</HTML>
"
200
#<HASH-TABLE :TEST EQUAL :COUNT 10 {1003B35F73}>
#<QURI.URI.HTTP:URI-HTTPS https://lisp.org/>
#<CL+SSL::SSL-STREAM for #<FD-STREAM for "socket 192.168.11.253:47632, peer: 144.76.156.38:443" {100361BC23}>>
```
このうち特に重要なのは最初の2つで、データ本体は次節で扱うHTMLパーサへの入力として用い、ステータスコードはデータの取得に成功したか失敗したか、さらにその理由を調べるのに用いる。

### XML/HTMLパーサ: Plump
ここで紹介するPlumpと次節で紹介するCLSSは同じ作者のプロダクトで、セットで使われる。これらはDexadorと同様にQuicklispからインストールできる。

```
(ql:quickload '(:plump :clss))
```

PlumpはXML/HTMLパーサであり、XML/HTMLデータを文字列として受け取り、CLOSオブジェクトのノードから構成される木構造(DOMツリー)を生成し、そのルートノードのオブジェクトを返す。

例えば以下のようにして、前述のDexadorの`get`の結果をパースすると、ルートノードのオブジェクトが返る。


```
(defparameter *root-node* (plump:parse *html-source*))

;; *root-node* => #<PLUMP-DOM:ROOT {1008638843}>
```

各ノードの子ノードのベクタは`plump:children`で、親ノードは`plump:parent`でそれぞれ得られる。


```
(plump:children *root-node*)

;; => #(#<PLUMP-DOM:ELEMENT HTML {1008AE94D3}> #<PLUMP-DOM:TEXT-NODE {1008AF2EF3}>)

(plump:parent (aref (plump:children *root-node*) 0))

;; => #<PLUMP-DOM:ROOT {1008AE8F93}>
```

また、DOMツリーを走査し、各ノードに対して関数を適用する高階関数`plump:traverse`が用意されている。
例えば、テキストノードから文字列を取り出して連結して返す関数は以下のようになる。
ここでは`traverse`に各ノードについて適用される関数を渡している。キーワードパラメータ`:test`で述語関数を渡すと、それを満足するノードに対してのみ第二引数の関数が適用される。

```
(defun concat-node-text (node)
  (let ((text-list nil))
    (plump:traverse node
                    (lambda (node) (push (plump:text node) text-list))
                    :test #'plump:text-node-p)
    (apply #'concatenate 'string (nreverse text-list))))

(concat-node-text *root-node*)
```

この結果は以下のようになるだろう。

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

このサイトをブラウザのインスペクタで調べると、内容のまとまりで`w-section`というクラスが付いていることが分かる。例えば、このクラス名でDOMツリーを検索することができる。なお、CSSと同様に、クラスの場合はクラス名の前に`.`を付け、IDの場合はID名の前に`#`を付ける。また、タグとの組み合わせで指定することもできる。

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

最近のWebブラウザにはインスペクタが搭載されており、Webページ上の調べたいところを右クリックして「検証」や「Inspect」などと表記されている項目を選べば、HTMLソース中の対応する部分がハイライトされる。そこからクラスやID、タグといったものを調べられる。また、HTMLソースの要素上で右クリックして「Copy > Copy selector」などを選べばその部分のCSSセレクタをクリップボードにコピーできる。これをそのまま`clss:select`に渡せば該当部分に対応するノードを得られる。

#### ログインを必要とするサイト

## 文書分類 / 文書クラスタリング

### 文書分類に必要なライブラリ
#### 形態素解析 mecab
#### 教師あり学習、線形分類器: cl-online-learning
#### 教師なし学習、NMF、テンソル分解: clml

### データセットの作成
#### TF-IDF
#### word2vec

### スパースなデータの取り扱い
