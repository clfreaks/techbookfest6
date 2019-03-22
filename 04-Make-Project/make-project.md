# プロジェクトの作り方

## 前提知識

### プロジェクト

Common Lispでプロジェクトというとき、多くの場合はライブラリのことを指します。オープンソースで公開されているライブラリの多くは、ASDFを用いて構築されています。ASDFでは、システムファイルにパッケージやファイルを規定の方法で記すことで、自動でライブラリをビルドします。

### シンボル

Common Lispにおいてシンボルは、名前、パッケージ、変数の値、関数、属性リストを含む一意のデータです。例として`something`という名前でシンボルを作成して、変数の値、関数、属性リストに値を設定して印字してみます。

```
* (defvar something 1)  ; 変数の値
* something
1

* (defun something ()(princ "something"))  ; "関数
* (something)
something
"something"

* (setf (get 'something 'old) "10years")  ; 属性リスト
"10years"
* (setf (get 'something 'color) "blue")
"blue"
* (symbol-plist 'something)
(COLOR "blue" OLD "10years")
```

このように、シンボルは見た目以上に多くの内容を含みます。


### パッケージ

Common Lispにおいてパッケージは、名前空間の役割を果たします。デフォルトのパッケージは`COMMON-LISP-USER`です。プロジェクト内でパッケージを切り替えることで、同じ名前のシンボルを共存させることができます。次の例では、デフォルトのCOMMON-LISP-USERパッケージの`greet関数`では"Hello"、JPNパッケージの`greet関数`では"こんにちは"と印字するように、greetという名前のシンボルを複数のパッケージで共存させます。

```
$ ros run
* (defpackage :JPN         ; パッケージJPNを定義する
    (:use :cl)             ; COMMON-LISPパッケージのシンボル定義を使う
    (:export :greet))      ; 他のパッケージから利用できるようにgreetをexportする
#<PACKAGE "JPN">
* (in-package :JPN)        ; パッケージをJPNに移動する
#<PACKAGE "JPN">
* (defun greet()
    (format nil "こんにちは"))　; JPNパッケージ内で関数を定義する
GREET
* (in-package :cl-user)       ; パッケージをCOMMON-LISP-USERに移動する
#<PACKAGE "COMMON-LISP-USER">
* (defun greet()
    (format nil "Hello"))     ; COMMON-LISP-USERパッケージ内で関数を定義する
* (greet)                     ; カレントパッケージでgreet関数を実行する。
"Hello"
* (jpn:greet)                 ; パッケージJPNのgreet関数を実行する。
"こんにちは"
```

このように、プロジェクトではシンボルが実行時にどのパッケージを参照するかを管理する必要があります。


## パッケージの管理方法

### packages.lisp

パッケージの管理方法にはいくつか方法がありますが、広く使われているパッケージの管理方法として、packages.lisp(もしくはpackage.lisp)を最初に読み込む手法があります。HTML生成ライブラリのCL-WHOは、この手法でパッケージが管理されています。defsystem内で`:serial t`と指定することで、components内のファイルを上から順に読み込んでいきます。

CL-WHOでは、`packages.lisp`が最初に読み込まれた後、`specials.lisp` `util.lisp` `who.lisp`が順に読み込まれていきます。

```
(asdf:defsystem :cl-who
  :description "(X)HTML generation macros"
  :version "1.1.4"
  :serial t
  :license "BSD"
  :components ((:file "packages")
               (:file "specials")
               (:file "util")
               (:file "who"))
```

`packages.lisp`では、次のように、パッケージが定義されています。`:use`の節では利用するパッケージを指定し、`:export`の節では、エクスポートされるシンボルを指定します。`#+:sbcl (:shadow :defconstant)`は、もし処理系がSBCLの場合、COMMON-LISPパッケージの`defconstant`を隠して(shadow)、CL-WHOで定義されている`defconstant`を利用するという指定です。CL-WHOの`defconstant`は specials.lisp で定義されています。

```
(in-package :cl-user)

(defpackage :cl-who
  (:use :cl)
  (:nicknames :who)
  #+:sbcl (:shadow :defconstant)
  #+:sb-package-locks (:lock t)
  (:export :*attribute-quote-char*
           :*empty-attribute-syntax*
	   (中略)
           :with-html-output
           :with-html-output-to-string))
```

packages.lisp以降に読み込まれるファイルが、`(in-package :cl-who)`で始まっているところに注目してください。`packages.lisp`以降に読み込まれるファイルでは、ファイルの冒頭で`packages.lisp`で定義されたパッケージに`in-package`することで、`packages.lisp`で定義したパッケージ(この場合はcl-who)にシンボルを登録していきます。

```
;; specials.lisp
(in-package :cl-who)

(defmacro defconstant (name value &optional doc)
  "Make sure VALUE is evaluated only once \(to appease SBCL)."
  `(cl:defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

;; util.lisp
(in-package :cl-who)

(defmacro n-spaces (n)
  "A string with N spaces - used by indentation."
  `(make-array ,n
               :element-type 'base-char
               :displaced-to +spaces+
               :displaced-index-offset 0))

;; who.lisp
(in-package :cl-who)

(defun html-mode ()
  "Returns the current HTML mode. :SGML for \(SGML-)HTML, :XML for
XHTML and :HTML5 for HTML5 (HTML syntax)."
  *html-mode*)
(以下省略)
```

CL-WHOではファイル数が少ないためこの手法が十分機能しますが、ライブラリの規模が大きくなり、管理するパッケージやファイルが多くなるにつれて、この手法でのパッケージ管理は複雑化します。より効率的にパッケージを管理するために、`one package per file`(1ファイルにつき1パッケージ)という手法が提唱されました。

### package-inferred-system

この手法では、全てのファイルはdefpackageで始まります。複数のパッケージ間の依存関係が明示的で、全てのファイルが固有のパッケージ名をもつので、ファイル間の依存関係は推測(inferred)されます。

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

package-inferred-systemを用いて、簡単なプロジェクトを作成してみましょう。zipcloudのWeb APIを利用して、郵便番号から地名を検索するシステム`yubin`を作ります。プロジェクト完成後の利用方法は次の通りです。

```
$ ros install t-cool/yubin
$ yubin 6380321
$ 奈良県吉野郡天川村坪内
```

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

Qlotは、プロジェクトごとにライブラリを管理するためのツールです。ライブラリによっては、開発途中にAPIが変更される可能性があります。Qlotでは、依存ライブラリの情報を`qlfile`に記載することで、プロジェクトフォルダ内の`quicklisp`フォルダ内にライブラリがダウンロードされて、ライブラリのバージョンを固定することができます。

試しに、qlfileを書いて試してみます。次のように、Github上のブランチ、Quicklispの月別レポジトリを指定することができます。

```
git dexador https://github.com/fukamachi/dexador :ref 031ee9935797e17d544710551d4b0c40a57a6180
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
* (ql:quickload :yubin)
* (yubin:get-place 6380321)
  奈良県吉野郡天川村坪内
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
