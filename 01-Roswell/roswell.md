# Roswell

### Roswellで何が嬉しいのか

　Common Lispには、ANSI Common Lispの仕様に準拠した処理系が複数あるが、それぞれインストール方法やシェルとの連携方法が異なる。Roswellは、処理系の違いを統一して扱うための開発ツールである。Roswellを用いると、Common Lispの開発環境の構築、アプリケーション開発、テスト、デプロイをコマンドラインからシンプルに行うことができる。

### Roswell以前のCommon Lisp

### ASDF

　ASDF（Another System Definition Facility）は、Common Lispのビルドツールである。主な処理系に組み込まれており、デフォルトで利用することができる。ASDFでは、システム定義ファイルに読み込むコードや依存ライブラリを書くことで、アプリケーションを統一した方法でビルドすることができる。ただし、依存関係にあるライブラリのダウンロードはしない。
  
### Quicklisp

　Quicklispは、ASDFをベースにしたライブラリ管理システムである。ライブラリのダウンロード、コンパイル、読み込みを自動化することができる。Zach Beane氏により登録済みのライブラリーが主な処理系での動作を確認のうえ、毎月最新版が公開されている。利用するには、次のように`quicklisp.lisp`をダウンロードして処理系から読み込んで初期化する必要がある。
 
```
$ curl -O https://beta.quicklisp.org/quicklisp.lisp
$ sbcl --load "quicklisp.lisp"
* (quicklisp-quickstart:install)
* (ql:add-to-init-file)
```
 
Quicklispでライブラリをインストールするには、REPLから`(ql:quickload :ライブラリ名)`とする。試しに、ユーティリティライブラリAlexandriaをインストールしてみよう。

```
* (ql:quickload :alexandria)
NIL
* To load "alexandria":
  Load 1 ASDF system:
    alexandria
; Loading "alexandria"

(:ALEXANDRIA)
```

　`(ql:quickload :<ライブラリ名>)`でロードした後は、`(ライブラリのパッケージ名:シンボル名)`でエクスポートされた関数やマクロを使うことができる。要素をシャッフルする`shuffle`関数を使うには次のようにする。

```
* (alexandria:shuffle '(1 2 3 4 5 6))

(6 3 5 2 4 1)
* 
```

　では、Roswellのインストールへと進む。RoswellにはQuicklispが組み込まれているため、Quicklispのダウンロードや初期化の手順は必要ない。

### Roswellのインストール

#### Homebrewを利用したインストール

LinuxとmacOSではHomebrewでインストールすることができる。

```shell-session
$ brew install roswell
```

#### ソースからインストールするには

Homebrewを使えないが、Roswellの開発に興味がある読者は、

* Cのコンパイル環境
* libcurlと、そのヘッダ
* automake/autoconf

これらの環境を準備したLinux/FreeBSD/macOSでコンパイルの上、インストールすることができる。

Debianベースの環境では、手順は次の通りである。

```
$ if which apt-get > /dev/null; then sudo apt-get -y install git build-essential automake libcurl4-openssl-dev;fi
$ git clone -b release https://github.com/roswell/roswell.git
$ cd roswell
$ sh bootstrap
$ ./configure
$ make
$ sudo make install
$ ros setup
```

詳細は、https://github.com/roswell/roswell/wiki/Installation で確認できる。

## 処理系のインストール

　Roswellは、複数の処理系から特定のバージョンをインストールできる。ここでは、SBCL(Steel Bank Common Lisp)をインストールする。

　最新版のSBCLをインストールするには`sbcl-bin`、ソースコードからビルドするには`-bin`をとる。

```
# SBCL(最新版)をインストールする
$ ros install sbcl-bin

# SBCL(最新版)をソースからビルドしてインストールする
$ ros install sbcl
```

　バージョンを指定してインストールするには、スラッシュの後にバージョンを指定する。

```
# SBCL(1.4.1)をインストールする
$ ros install sbcl-bin/1.4.1

# SBCL(1.4.1)をソースからビルドしてインストールする
$ ros install sbcl/1.4.1
```

　`ros list installed`でインストール済みの処理系を確認できる。また、`ros use <処理系/バージョン>`で、処理系を切り替えることができる。


```
# インストール済みの処理系を確認
$ ros list installed
Installed versions of sbcl-bin:
sbcl-bin/1.4.1
sbcl-bin/1.4.16

#　処理系の切り替え
$ ros use sbcl-bin/1.4.1
```

　`ros run`でREPLを起動することができる。

```
$ ros run
* (princ "hello, world")
hello, world
"hello, world"
* 
```
## REPLの起動

　`ros run`でREPLを起動、`C-d`もしくは`(quit)`で終了することができる。

```
$ ros run
* (format nil "Hello")

"Hello"
* (quit)
$
```

## Roswell Script

　Roswell Scriptを用いると、シェルコマンドであることを意識せずにCommon Lispでスクリプトを書くことができる。

### ひな形を作成する

　ターミナルで`ros init <スクリプト名.ros>`とすると、Roswell Scriptの雛形が生成される。

```
$ ros init fact.ros
Successfully generated: fact.ros
```

　次のようなファイルが生成される。

```common-lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  ;;#+quicklisp(ql:quickload '() :silent t)
  )

(defpackage :ros.script.hoge.3759651958
  (:use :cl))
(in-package :ros.script.hoge.3759651958)

(defun main (&rest argv)
  (declare (ignorable argv)))
;;; vim: set ft=lisp lisp:
```

　では、階乗(factorial)を計算する関数`fact`を定義してみよう。
 
```common-lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  ;;#+quicklisp(ql:quickload '() :silent t)
  )

(defpackage :ros.script.hoge.3759651958
  (:use :cl))
(in-package :ros.script.hoge.3759651958)

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (1- n)))))

(defun main (n &rest argv)
  (declare (ignore argv))
  (format t "~&Factorial ~D = ~D~%" n (fact (parse-integer n))))
```

### Roswell Scriptの実行

`<ファイル名>.ros 引数`の形式でmain関数に引数が渡されて実行される。

```
$ fact.ros 3
Factorial 3 = 6

$ fact.ros 10
Factorial 10 = 3628800
```

## ros build

`ros build`では、Roswell Scriptをビルドして実行ファイルを生成することができる。fact関数をビルドして、ビルドの前後で実行時間を比較してみよう。

```
# build前
$ ros fact.ros 10
Factorial 10 = 3628800
$ time ros fact.ros 10
Factorial 10 = 3628800

real	0m0.786s
user	0m0.628s
sys	0m0.133s

# fact.rosをbuildする
$ ros build fact.ros
compressed 0 bytes into 8 at level -1
compressed 32768 bytes into 401 at level -1
compressed 31719424 bytes into 7068417 at level -1
compressed 2097152 bytes into 562396 at level -1
compressed 12910592 bytes into 3890295 at level -1

# build後
$ time fact 10
Factorial 10 = 3628800

real	0m0.187s
user	0m0.156s
sys	0m0.026s
```

build後、高速化したことが分かる。

## ライブラリのインストール

　Roswellの`ros install`でライブラリをインストールすると、ローカル環境の`~/.roswell/local-projects/`以下にソースコードがダウンロードされてインストールされる。デフォルトでは`~/.roswell/local-projects/`からライブラリが読み込まれる。RoswellのREPLでライブラリを読み込むには、`(ql:quickload :ライブラリ名`)とする。

　試しに`ros install`でClackをインストールしてみよう。

```
$ ros install fukamachi/clack
Installing from github fukamachi/clack
To load "clack":
  Load 1 ASDF system:
    clack
; Loading "clack"
..................................................
[package lack.component]..........................
[package nibbles].................................
[package ironclad]................................
[package ironclad-vm].............................
[package lack.util]...............................
[package lack.builder]............................
[package lack]....................................
[package lack.middleware.backtrace]...............
[package alexandria.0.dev]........................
[package bordeaux-threads]........................
[package clack.util]..............................
[package clack.handler]...........................
　(省略)
Found 1 scripts: clackup
/Users/noguchihiroki/.roswell/bin/clackup
up to date. stop
$ 
``` 

## .roswell/bin

　`ros install <ライブラリ名>`としてライブラリをインストールすると、プロジェクトの`roswell`フォルダにあるRoswell Scriptが`~/.roswell/bin`にコピーされる。`~/.bashrc`等で次のようにPATHを通しておくことで、`~/.roswell/bin`内にあるRoswell Scriptをターミナルのコマンドとして使うことができる。

```
export PATH=$PATH:~/.roswell/bin
```

　Clackのプロジェクトでは、プロジェクト・トップの`roswell`フォルダの中に`clackup.ros`が入っている。`ros install fukamachi/clack`とすることで、`clackup.ros`が`~/.roswell/bin/clackup`にコピーされる。インストール後は`clackup`コマンドが使えるようになる。試しに、次のコードを`app.lisp`に保存して、`clackup`コマンドを実行してみよう

```
(lambda (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("Hello, Clack!")))
```

```
$ clackup app.lisp
Hunchentoot server is going to start.
Listening on localhost:5000.
```

ブラウザから http://localhost:5000 にアクセスすると、Clackサーバーが起動したのが確認できる。

## ライブラリの更新

　ライブラリを最新版に更新するには、`ros update <ライブラリ名>`とする。

```
$ ros update clack
git pull on /Users/t-cool/.roswell/local-projects/fukamachi/clack/
find: lib: No such file or directory
Already up to date.
[1/3] System 'clack' found. Loading the system..
[2/3] Processing build-hook..
[3/3] Attempting to install the scripts in roswell/ subdirectory of the system...
/Users/t-cool/.roswell/bin/clackup
up to date. stop
```

## Qlot

　Qlotは、プロジェクトごとにライブラリを管理するためのツールである。Quicklispに登録されているライブラリによっては、開発途中にAPIが変更される可能性がある。Qlotでは、依存ライブラリの情報を`qlfile`に記載することで、プロジェクトフォルダ内の`quicklisp`フォルダ内にライブラリがダウンロードされて、ライブラリのバージョンを固定することができる。

　試しに、qlfileを書いて試してみよう。次のように、Github上のブランチ、Quicklispの月別レポジトリを指定することができる。

```
git clack https://github.com/fukamachi/clack.git
github datafly fukamachi/datafly :branch jojo
ql log4cl 2014-03-17
```

　qlfileがあるディレクトリ内で`qlot install`とすると、プロジェクト内のquicklispフォルダにソースコードがダウンロードされた後、インストールされる。

```
$ qlot install
```

　インストール後、quicklispフォルダを開くと、dists以下にライブラリのソースコード、binフォルダにRowell Scriptが入っているのが確認できる。プロジェクト内のquicklispフォルダからライブラリをロードするには`qlot exec`を用いる。`qlot exec ros -S . run`とすれば、REPLが起動する。

```
$ qlot exec ros -S . run
* 
```

　`qlot exec <Roswell Script>`とすると、プロジェクト内の`quicklisp/bin`からRoswell Scriptのコマンドを実行することができる。

```
$ qlot exec clackup app.lisp
```

## まとめ

* ASDFはCommon Lispのビルドツールであり、QuicklispはASDFを元に構築されている。

* RoswellではデフォルトでQuicklispを利用できる。

* 処理系をインストールするには`ros install <処理系>`とする。

* REPLを起動するには`ros run`とする。

* Roswell Scriptを用いると、Common Lispでシェルコマンドを書くことができる。

* `ros build`では、Roswell Scriptをビルドして実行ファイルを生成することができる。

* ライブラリをインストールするには`ros install Githubアカウント名/レポジトリ名`とする。

* プロジェクト・トップの`roswell`フォルダにRoswell Scriptを入れておくと、Roswell Scriptをターミナルのコマンドとして使うことができる。

* ライブラリを最新版に更新するには、`ros update <ライブラリ名>`とする。

* Qlotを用いると、プロジェクトごとにライブラリのバージョンを管理することができる。
