# Roswell

本章はRoswellについて解説します。Roswellは複数のCommon Lispの処理系のインストール、管理、切り替え、異なるコマンドラインオプションの共通化等を行なう処理系マネージャです。2014年から開発を開始した、比較的新しいツールであるため、ツールの作者としては悲しいことに、Common Lispのコミュニティの中にはこんなものは不要だろうという意見もあります。Roswellの使い方の一通りがCommon Lispの使い方の説明になるため、他の章に入る前にRoswellの使いかたについて説明します。

### Roswellのインストール

#### Homebrewを利用したインストール

LinuxとmacOSではHomebrewでインストールすることができます。

```shell-session
$ brew install roswell
```

#### ソースからインストールする

Homebrewが使えない環境での、Roswellのインストールには

* Cのコンパイル環境
* libcurlと、そのヘッダ
* automake/autoconf

これらの環境を準備したLinux/FreeBSD/macOSでコンパイルの上、インストールすることができます。

Debianベースの環境では、手順は次の通りです。

```
$ if which apt-get > /dev/null; then sudo apt-get -y install git build-essential \
 automake libcurl4-openssl-dev;fi
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

Roswell Scriptを用いると、Common Lispでコマンドラインから利用できるスクリプトを書くことができます。

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

`ros install <GitHubアカウント名/レポジトリ名>`とすると、GitHubのレポジトリからダウンロードされます。　

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

`ros update <ライブラリ名>`とすることで、ライブラリをGitHub上の最新版に更新することができます。
 
```
$ ros update clack
```

## まとめ

* Roswellで処理系をインストールするには`ros install <処理系>`とする。

* RoswellでREPLを起動するには`ros run`とする。

* Roswell Scriptを用いると、Common Lispでシェルコマンドを書くことができる。

* ros buildでは、Roswell Scriptをビルドして実行ファイルを生成することができる。

* ライブラリをGithubからインストールするには`ros install Githubアカウント名/レポジトリ名`とする。

* プロジェクト・トップのroswellフォルダにRoswell Scriptを入れておくと、Roswell Scriptをターミナルのコマンドとして使うことができる。

* ライブラリを最新版に更新するには、ros update <ライブラリ名>とする。
