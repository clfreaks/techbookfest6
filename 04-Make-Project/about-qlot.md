# プロジェクトの作り方

　Common Lispで「プロジェクト」や「システム」という用語を使うときは、多くの場合、Common Lispで構築されたライブラリを指します。オープンソースで公開されているライブラリの多くは、ASDFを用いて構成されています。ASDFは、asd拡張子のシステムファイルにシステムを定義することで、ライブラリを自動でビルドします。

　システムの定義にはいくつか方法がありますが、ここでは`package-inferred-system`と呼ばれる手法を紹介します。

### package-inferred-system

　パッケージ群を定義するための手法として、`one package per file`(1ファイルにつき1パッケージ)と名づけられた手法があります。この手法では、全てのファイルはdefpackageで始まります。複数のパッケージ間の依存関係が明示的で、全てのファイルが固有のパッケージ名をもつので、ファイル間の依存関係は推測(inferred)されます。
 

　WebフレームワークのUtopianでは、この手法でシステムが構築されています。`utopian.asd`の`depends-on`に注目してください。"utopian/package"と指定されているのは、utpianフォルダー内のpackage.lispを示します。このように、パッケージ名をファイルのパスと対応させて管理するのも特徴の1つです。

```
(defsystem "utopian"
  :description "Full stack web application framework"
  :depends-on ("utopian/package" "lack" "mito" "bordeaux-threads"))
```

　package.lispを開くと、次のように定義されてます。:use-reexportの`#:utopian/db`は、utopianフォルダにあるdb.lispを示します。


```
(uiop:define-package #:utopian/package
  (:nicknames #:utopian)
  (:use-reexport #:utopian/view
                 #:utopian/config
                 #:utopian/db
		 ;; 以下省略
		 ))
```

　`db.lisp`では、次のようにパッケージが定義されています。defpackage内で、依存するパッケージを`(:import-from #:ライブラリ名)`の形式で指定します。ライブラリから指定のシンボルのみを取り込むときは、`(:import-from #:ライブラリ名 #:シンボル名1 #:シンボル名2)`とします。

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

　このようにシステムを定義後、Quicklispでシステムを読み込むと、依存関係にあるライブラリがダウンロードされて順番に読み込まれます。

## プロジェクト作成の例 - 地名検索システムyubin

　プロジェクトの例として、郵便番号から地名を検索するシステム`yubin`を作ります。プロジェクト完成後の利用方法は次の通りです。

```
$ ros install t-cool/yubin
$ yubin 6380321
$ 奈良県吉野郡天川村坪内
```

### 郵便番号検索API

http://zipcloud.ibsnet.co.jp/doc/api

郵便番号検索APIは、日本郵便が公開している郵便番号データを検索する機能をRESTで提供しています。

リクエストURL
ベースとなるURLは以下になります。

http://zipcloud.ibsnet.co.jp/api/search

このURLにリクエストパラメータを加え、リクエストを行います。

（例）郵便番号「7830060」で検索する場合
http://zipcloud.ibsnet.co.jp/api/search?zipcode=7830060

レスポンスフィールド
フィールド名	項目名	備考
status	ステータス	正常時は 200、エラー発生時にはエラーコードが返される。
message	メッセージ	エラー発生時に、エラーの内容が返される。
results	--- 検索結果が複数存在する場合は、以下の項目が配列として返される ---
zipcode	郵便番号	7桁の郵便番号。ハイフンなし。
prefcode	都道府県コード	JIS X 0401 に定められた2桁の都道府県コード。
address1	都道府県名	
address2	市区町村名	
address3	町域名	
kana1	都道府県名カナ	
kana2	市区町村名カナ	
kana3	町域名カナ	

### yubin.asd

　システムファイルを次のように書きます。package-inferred-systemを使うことを明示するために、`:class :package-inferred-system`を追加します。`:depends-on`で、mainファイルへのパスを書きます。

```
(defsystem "yubin"
  :class :package-inferred-system
  :depends-on (yubin/main))  
```

### main.lisp

　ここでは、JSONのパーズのためにJonathanのparse関数、HTTP通信のためにDexadorのget関数をインポートします。

```
(defpackage #:yubin/main
  (:import-from #:jonathan
		#:parse)
  (:import-from #:dexador
		#:get)
  (:export #:get-place))
(in-package #:yubin/main)

(defun get-place (zipcode)      
  (let* ((url (format nil "http://zipcloud.ibsnet.co.jp/api/search?zipcode=~A" zipcode))
	 (data (reverse (car (fourth (jonathan:parse (dex:get url))))))
	 (place (concatenate 'string (first data)(third data) (fifth data))))
    (format t "~A" place)))
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
  #+quicklisp(ql:quickload '(yubin) :silent t)
  )

(defpackage :ros.script.yubin.3761982565
  (:use :cl))
(in-package :ros.script.yubin.3761982565)

(defun main (zipcode &rest argv)
  (declare (ignorable argv))
  (yubin:get-place zipcode))
```
 
　これでライブラリは完成です。では、完成したライブラリをインストールして使ってみましょう。

```
$ ros install t-cool/yubin
$ yubin 6380321
  奈良県吉野郡天川村坪内
```

## Qlot

　Qlotは、プロジェクトごとにライブラリを管理するためのツールです。Quicklispに登録されているライブラリによっては、開発途中にAPIが変更される可能性があります。Qlotでは、依存ライブラリの情報を`qlfile`に記載することで、プロジェクトフォルダ内の`quicklisp`フォルダ内にライブラリがダウンロードされて、ライブラリのバージョンを固定することができます。

　試しに、qlfileを書いて試してみます。次のように、Github上のブランチ、Quicklispの月別レポジトリを指定することができます。

```
git dexador https://github.com/fukamachi/dexador.git
ql jonathan 2018-12-10
```

　qlfileがあるディレクトリ内で`qlot install`とすると、プロジェクト内のquicklispフォルダにソースコードがダウンロードされた後、インストールされます。

```
$ qlot install
```

　インストール後、quicklispフォルダを開くと、dists以下にライブラリのソースコード、binフォルダにRowell Scriptを確認できます。
 
　プロジェクト内のquicklispフォルダからライブラリをロードするには`qlot exec`を用いる。`qlot exec ros -S . run`とすれば、REPLが起動します。

```
$ qlot exec ros -S . run
* 
```

　`qlot exec <Roswell Script>`とすると、プロジェクト内の`quicklisp/bin`からRoswell Scriptのコマンドを実行することができます。

```
$ qlot exec yubin 6380321
  奈良県吉野郡天川村坪内
```

## まとめ

* オープンソースで公開されているライブラリの多くは、ASDFを用いて構成されている。

* システムの定義の1つ`package-inferred-system`では、ファイルごとにパッケージを定義する。

* プロジェクトでroswell/binコマンドを使えるようにするには、roswellディレクトリ内にRoswell Scriptを書く。

* Qlotを用いると、プロジェクトごとにライブラリのバージョンを管理することができる。
