# Roswell

### Roswellで何が嬉しいのか

Common Lispには、ANSI Common Lispの仕様に準拠した処理系が複数あり、それぞれインストール方法やシェルとの連携方法が違います。Roswellは、処理系の違いを統一して扱うための開発ツールです。Roswellを用いると、Common Lispの開発環境の構築、アプリケーション開発、テスト、デプロイをコマンドラインからシンプルに行うことができます。

### Roswell以前のCommon Lisp

### ASDF

ASDF（Another System Definition Facility）は、Common Lispのビルドツールです。主な処理系に組み込まれており、デフォルトで利用することができます。ASDFでは、システム定義ファイルに読み込むコードや依存ライブラリを書くことで、アプリケーションを統一した方法でビルドします。ただし、依存関係にあるライブラリのダウンロードは行いません。
  
### Quicklisp

Quicklispは、ASDFをベースにしたライブラリ管理システムです。ライブラリのダウンロード、コンパイル、読み込みを自動で行います。Zach Beane氏により登録済みのライブラリーが主な処理系での動作を確認のうえ、毎月最新版が公開されています。利用するには、次のように`quicklisp.lisp`をダウンロードして処理系から読み込んで初期化する必要があります。
 
```
$ curl -O https://beta.quicklisp.org/quicklisp.lisp
$ sbcl --load "quicklisp.lisp"
* (quicklisp-quickstart:install)
* (ql:add-to-init-file)
```
 
Quicklispでライブラリをインストールするには、REPLから`(ql:quickload :ライブラリ名)`とします。試しに、ユーティリティライブラリAlexandriaをインストールしてみます。

```
* (ql:quickload :alexandria)
NIL
* To load "alexandria":
  Load 1 ASDF system:
    alexandria
; Loading "alexandria"

(:ALEXANDRIA)
```

`(ql:quickload :<ライブラリ名>)`でロードした後は、`(ライブラリのパッケージ名:シンボル名)`でエクスポートされた関数やマクロを使うことができます。要素をシャッフルする`shuffle`関数を使うには次のようにします。

```
* (alexandria:shuffle '(1 2 3 4 5 6))

(6 3 5 2 4 1)
* 
```

では、Roswellをインストールしましょう。RoswellにはQuicklispが組み込まれているため、Quicklispのダウンロードや初期化の手順は必要ありません。

### Roswellのインストール

#### Homebrewを利用したインストール

LinuxとmacOSではHomebrewでインストールすることができます。

```shell-session
$ brew install roswell
```

#### ソースからインストールするには

Homebrewは使わないが、Roswellの開発に興味がある読者は、

* Cのコンパイル環境
* libcurlと、そのヘッダ
* automake/autoconf

これらの環境を準備したLinux/FreeBSD/macOSでコンパイルの上、インストールすることができます。

Debianベースの環境では、手順は次の通りです。

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

詳細は、https://github.com/roswell/roswell/wiki/Installation で確認できます。

## 処理系のインストール

Roswellは、複数の処理系から特定のバージョンをインストールできる。ここでは、SBCL(Steel Bank Common Lisp)をインストールします。

最新版のSBCLをインストールするには`sbcl-bin`、ソースコードからビルドするには`-bin`をとります。

```
# SBCL(最新版)をインストールする
$ ros install sbcl-bin

# SBCL(最新版)をソースからビルドしてインストールする
$ ros install sbcl
```

バージョンを指定してインストールするには、スラッシュの後にバージョンを指定します。

```
# SBCL(1.4.1)をインストールする
$ ros install sbcl-bin/1.4.1

# SBCL(1.4.1)をソースからビルドしてインストールする
$ ros install sbcl/1.4.1
```

`ros list installed`でインストール済みの処理系を確認できます。また、`ros use <処理系/バージョン>`で、処理系を切り替えることができます。


```
# インストール済みの処理系を確認
$ ros list installed
Installed versions of sbcl-bin:
sbcl-bin/1.4.1
sbcl-bin/1.4.16

#　処理系の切り替え
$ ros use sbcl-bin/1.4.1
```

`ros run`でREPLを起動することができます。

```
$ ros run
* (princ "hello, world")
hello, world
"hello, world"
* 
```
## REPLの起動

`ros run`でREPLを起動、`C-d`もしくは`(quit)`で終了することができます。

```
$ ros run
* (format nil "Hello")

"Hello"
* (quit)
$
```

## Roswell Script

Roswell Scriptを用いると、シェルコマンドであることを意識せずにCommon Lispでスクリプトを書くことができます。

### ひな形を作成する

ターミナルで`ros init <スクリプト名.ros>`とすると、Roswell Scriptの雛形が生成されます。

```
$ ros init fact.ros
Successfully generated: fact.ros
```

生成された雛形を元に、階乗(factorial)を計算する関数`fact`を定義してみます。
 
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

`ros <スクリプト名.ros> 引数`の形式でmain関数に引数が渡されて実行されます。

```
$ ros fact.ros 10
Factorial 10 = 3628800
```

### ros build (実行ファイルの作成)

`ros build`では、Roswell Scriptをビルドして実行ファイルを生成することができます。`ros build <Roswell Script名>`とすると、`.ros`拡張子が消えて、実行ファイルが生成されます。

では、fact関数をビルドしてみます。

```
$ ros build fact.ros
compressed 0 bytes into 8 at level -1
compressed 32768 bytes into 401 at level -1
compressed 31719424 bytes into 7068417 at level -1
compressed 2097152 bytes into 562396 at level -1
compressed 12910592 bytes into 3890295 at level -1
```

ビルド前後の実行時間を比較すると、ビルド後は5倍ほど高速化したことがわかります。

```
# ビルド前
$ time ros fact.ros 10
Factorial 10 = 3628800

real	0m0.786s
user	0m0.628s
sys	0m0.133s

# ビルド後
$ time fact 10
Factorial 10 = 3628800

real	0m0.187s
user	0m0.156s
sys	0m0.026s
```

## ライブラリのインストール

`ros install`でライブラリのインストールを行います。

`ros install <ライブラリ名>`とすると、Quicklispのアーカイブからダウンロードされます。

```
$ ros install vecto
To load "vecto":
  Load 3 ASDF systems:
    cl-vectors zpb-ttf zpng
  Install 1 Quicklisp release:
    vecto
Downloading http://beta.quicklisp.org/archive/vecto/2017-12-27/vecto-1.5.tgz
##########################################################################
; Loading "vecto"
(以下、省略)
```

`ros install <Githubアカウント名/レポジトリ名>`とすると、Githubのレポジトリからダウンロードされます。　

試しにClackをインストールします。

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
/Users/t-cool/.roswell/bin/clackup
up to date. stop
$ 
``` 

`ros install`でライブラリをインストールすると、ローカル環境の`~/.roswell/local-projects/`以下にソースコードがダウンロードされてインストールされます。デフォルトでは`~/.roswell/local-projects/`からライブラリが読み込まれます。RoswellのREPLでライブラリを読み込むには、`(ql:quickload :ライブラリ名`)とします。

## .roswell/bin

`ros install <ライブラリ名>`としてライブラリをインストールすると、プロジェクトの`roswell`フォルダにあるRoswell Scriptが`~/.roswell/bin`にコピーされます。`~/.bashrc`等で次のようにPATHを通しておくことで、`~/.roswell/bin`内にあるRoswell Scriptをターミナルのコマンドとして使うことができます。

```
export PATH=$PATH:~/.roswell/bin
```

Clackのプロジェクトでは、プロジェクト・トップの`roswell`フォルダの中に`clackup.ros`が入っています。`ros install fukamachi/clack`とすることで、`clackup.ros`が`~/.roswell/bin/clackup`にコピーされます。インストール後は`clackup`コマンドが使えるようになります。試しに、次のコードを`app.lisp`に保存して、`clackup`コマンドを実行してみます。

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

ブラウザから http://localhost:5000 にアクセスすると、Clackサーバーが起動したのが確認できます。

### ライブラリの更新

`ros update <ライブラリ名>`とすることで、ライブラリをGithub上の最新版に更新することができます。
 
```
$ ros update cl-ignition
git pull on /Users/t-cool/.roswell/local-projects/dbym4820/cl-ignition/
find: lib: No such file or directory
remote: Enumerating objects: 30, done.
remote: Counting objects: 100% (30/30), done.
remote: Compressing objects: 100% (11/11), done.
remote: Total 22 (delta 14), reused 16 (delta 9), pack-reused 0
Unpacking objects: 100% (22/22), done.
From https://github.com/dbym4820/cl-ignition
   2a91314..4d385a5  master     -> origin/master
Updating 2a91314..4d385a5
Fast-forward
 LICENSE              |  2 +-
 cl-ignition.asd      |  3 ++-
 example/example.lisp | 22 +++++++++++++---
 src/cl-ignition.lisp | 11 +++++++-
 src/dbpedia.lisp     | 58 +++++++++++++++++++++++++++++++++++++++++++
 src/query.lisp       |  5 +++-
 6 files changed, 94 insertions(+), 7 deletions(-)
 create mode 100644 src/dbpedia.lisp
(以下省略)
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
