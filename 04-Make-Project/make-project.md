# プロジェクトの作り方

## プロジェクト

Common Lispでプロジェクトというとき、多くの場合はライブラリのことを指します。オープンソースで公開されているライブラリの多くは、ASDFを用いて構築されています。ASDFでは、規定の方法でシステムファイルを書くことで自動でライブラリをビルドします。

## package-inferred-system

`package-inferred-system`を用いたプロジェクトの作り方を紹介します。この手法は、全てのファイルはdefpackageで始まり固有のパッケージ名をもつことから、`one package per file`(1つのファイルにつき1パッケージ)と呼ばれます。パッケージ名をファイルのパス名と合致するように作成して、ファイル内でパッケージの依存関係を記述することで、パッケージ間の依存関係が推測(inferred)されて解決されます。

WebフレームワークUtopianでは、この手法でシステムが構築されています。`utopian.asd`で`utopian/package`と指定されているのは、utpianフォルダーのpackage.lispを示します。

```
(defsystem "utopian"
  :description "Full stack web application framework"
  :depends-on ("utopian/package" "lack" "mito" "bordeaux-threads"))
```

`package.lisp`の中を見ましょう。`#:utopian/db`は、utopianフォルダのdb.lispを示します。

```
(uiop:define-package #:utopian/package
  (:nicknames #:utopian)
  (:use-reexport #:utopian/view
                 #:utopian/config
                 #:utopian/db
		 ;; 以下省略 ))
```

`db.lisp`では、次のようにパッケージが定義されています。

```
(defpackage #:utopian/db
  (:use #:cl)
  (:import-from #:utopian/config
                #:config)
  (:import-from #:cl-dbi
                #:connect-cached)
  (:import-from #:mito
                #:*connection*)
  (:export #:db
           #:connection-settings
           #:with-connection))
```

defpackage内で依存するパッケージを`(:import-from #:ライブラリ名)`の形式で指定します。ライブラリから指定のシンボルのみを取り込むときは、`(:import-from #:パッケージ名 #:シンボル名)`とします。上記の場合は、`utopian/config.lisp`の`config`、`cl-dbi`の`connect-cached`、`mito`の`*connection*`がインポートされます。

このようにシステムを定義後、Quicklispでシステムを読み込むと、依存関係にあるライブラリがダウンロードされて順番に読み込まれます。

//embed[latex]{
\clearpage
//}

## Qlot

Qlotは、プロジェクトごとにライブラリを管理するためのツールです。Qlotでは、依存ライブラリの情報を`qlfile`に記載することで、プロジェクトフォルダ内の`quicklisp`フォルダ内にライブラリがダウンロードされて、ライブラリのバージョンを固定することができます。

同じマシンで複数のプロジェクトの開発を行うとき、それぞれのプロジェクトで依存ライブラリのバージョンが異なると、Quicklispだけでは管理しきれません。Qlotを用いると、複数人で開発するとき、それぞれの環境で依存ライブラリのバージョンをあわせることができます。

qlfileは、Node.jsのpackage.json、RubyのGemfileのような働きをします。Nodejsではpackage.jsonで指定したバージョンがnode\_modulesにダウンロードされますが、Qlotではqlfileでの指定バージョンがquicklispフォルダにダウンロードされます。

![Qlot](https://github.com/clfreaks/techbookfest6/blob/master/images/04-qlot.png)

次のチャプターで、実際にプロジェクト内でQlotを用いて、Qlotの使い方を説明します。

//embed[latex]{
\clearpage
//}

## プロジェクト作成の例 - 地名検索システムyubin

package-inferred-systemを用いて、簡単なプロジェクトを作成します。zipcloudのWeb APIを用いて、郵便番号から地名を検索するシステムを作ります。プロジェクトのファイル構成は次の通りです。

```
yubin
├── yubin.asd
├── main.lisp
├── roswell
│   └── yubin.ros
└── qlfile
```

Roswellでyubinをインストール後、次のように利用できます。

```
$ ros install t-cool/yubin
$ yubin 6380321
$ 奈良県吉野郡天川村坪内
```

では、システムファイル`yubin.asd`、メインファイルの`main.lisp`、Roswell Scriptの`yubin.ros`の内容、Qlotを用いたライブラリのバージョン管理法についてみていきます。

### yubin.asd

package-inferred-systemを使うことを明示するために、システムファイルに`:class :package-inferred-system`を追加します。`:depends-on`で`main.lisp`へのパスを書きます。

```
(defsystem "yubin"
  :class :package-inferred-system
  :depends-on (#:yubin/main))  
```

### main.lisp

ここでは、JSONのパースにJonathanのparse関数、HTTPのGETメソッドにDexadorのget関数をインポートします。Dexadorのget関数がデフォルトパッケージのgetと名前衝突を起こすため、shadowing-importを行います。

```
(defpackage #:yubin/main
  (:use #:cl)
  (:import-from #:jonathan
		#:parse)
  (:shadowing-import-from #:dexador
			  #:get)
  (:export #:get-place))
(in-package #:yubin/main)

(defun get-place (zipcode)      
　(let* ((url (format nil "http://zipcloud.ibsnet.co.jp/api/search?zipcode=~A" zipcode))
　　　　　(data (reverse (car (fourth (jonathan:parse (dex:get url))))))
　　　　　(place (concatenate 'string (first data)(third data) (fifth data))))
　　(format t "~A~%" place)))
```

### roswell/yubin.ros

roswellからget-place関数を使えるように、Roswellスクリプトを書きます。
 
```
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp(ql:quickload '(yubin) :silent t))
(defpackage :ros.script.yubin.3761982565
  (:use :cl))
(in-package :ros.script.yubin.3761982565)

(defun main (zipcode &rest argv)
  (declare (ignorable argv))
  (yubin/main:get-place zipcode))
```
 
### qlfile

このプロジェクトでは、外部ライブラリとしてDexadorとJonathanを用いています。yubinのプロジェクト内にqlfileを作成して、各ライブラリのバージョンを固定します。次のように、qlfileを作成して編集します。

```
git dexador https://github.com/fukamachi/dexador.git
ql jonathan 2018-12-10
```

Lemの起動後、`M-x slime`でREPLを起動します。`ql:quickload`でQlotの読み込み後、`qlot:install`でプロジェクトをインストールします。

```
CL-USER> (ql:quickload :qlot)
CL-USER> (qlot:install :yubin)
```

インストール後、プロジェクト内のquicklispフォルダにライブラリがダウンロードされ、qlfile.lockファイルが作成されます。qlfile.lockは、インストールした内容をスナップショットとして記録したものです。このファイルがあると`qlot:install`は`qlfile.lock`を優先します。開発者間でqlfile.lockを共有し、各環境で`qlot:install`を実行することで、依存ライブラリのバージョンを統一することができます。

#### プロジェクトをロードする

`qlot:quickload`を使うと、プロジェクト内のquicklispからライブラリをロードします。

```
CL-USER> (qlot:quickload :yubin)
CL-USER> (yubin/main:get-place 6380321)
奈良県吉野郡天川村坪内
```

#### ライブラリをアップデートする

ライブラリのバージョンを更新するためにqlfileを変更したときは`qlot:update`を実行します。

```
CL-USER> (qlot:update :yubin)
```

これで、プロジェクト内の`quicklisp`以下とqlfile.lockが更新されます。

## まとめ

* オープンソースで公開されているライブラリの多くは、ASDFを用いて構成されている。

* `package-inferred-system`を用いたパッケージ管理では、ファイルごとにパッケージを管理する。

* Qlotを使うと、複数人で開発するときに、各々の環境で依存ライブラリのバージョンをあわせることができる。
