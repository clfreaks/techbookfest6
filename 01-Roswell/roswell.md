# 環境構築「Roswell」

本章はRoswellについて解説します。Roswellは複数のCommon Lispの処理系のインストール、管理、切り替え、異なるコマンドラインオプションの共通化等を行なう処理系マネージャです。2014年から開発を開始した、比較的新しいツールであるため、ツールの作者としては悲しいことに、Common Lispのコミュニティの中にはこんなものは不要だろうという意見もあります。Roswellの使い方の一通りがCommon Lispの使い方の説明になるため、他の章に入る前にRoswellの使いかたについて説明します。

## Roswellのインストール

### Homebrewを利用したインストール

LinuxとmacOSではHomebrewでインストールすることができます。

```shell-session
$ brew install roswell
```

### Windowsでのインストール

Windows環境でのインストール用にはバイナリファイルを準備しています。

```
https://github.com/roswell/roswell/wiki/Installation#windows
```

解凍したディレクトリ下に`ros shell`を実行すると長いセットアップ時間の後にmsysが含まれた環境が提供されます。
本書の内容がWindowsに向けて書かれているわけではないので、本書の共としての推奨はしませんが、Windowsネイティブな開発がしたい場合には思い出してもらえると良いかと思います。

### ソースからインストールする

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

配布されている最新のSBCLをインストールするには`sbcl-bin`、ビルドをするには`sbcl`です。

```
# SBCL(アーキテクチャ向けに配布されている最新のバイナリバージョン)をインストールする
$ ros install sbcl-bin

# SBCL(最新版)をソースからビルドしてインストールする
$ ros install sbcl
```

バージョンを指定してインストールするには、スラッシュの後にバージョンを指定します。

```
# SBCL(1.4.1)のバイナリをインストールする
$ ros install sbcl-bin/1.4.1

# SBCL(1.4.1)をビルドしてインストールする
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

`ros run`でREPLを起動、`(quit)`で終了することができます。

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
ここでは、テストの章で利用するroveをインストールしましょう。


```
$ ros install rove
To load "rove":
  Load 1 ASDF system:
    asdf
  Install 1 Quicklisp release:
    rove
Downloading http://beta.quicklisp.org/archive/rove/2019-03-07/rove-20190307-git.tgz
##########################################################################
; Loading "rove"
[package rove/core/result]........................
(中略)
[3/3] Attempting to install the scripts in roswell/ subdirectory of the system...
Found 1 scripts: rove
/home/snmsts/.roswell/bin/rove
up to date. stop

```

`ros install <GitHubアカウント名/レポジトリ名>`とすると、GitHubのレポジトリからダウンロードされます。　

ここでroveをgithubからインストールします。

```
$ ros install fukamachi/rove
Installing from github fukamachi/rove
To load "rove":
  Load 1 ASDF system:
    rove
; Loading "rove"
[package rove/core/result]........................
(中略)
Found 1 scripts: rove
/home/snmsts/.roswell/bin/rove
up to date. stop
```

`ros install`でライブラリをインストールすると、ローカル環境の`~/.roswell/local-projects/`以下にソースコードがダウンロードされてインストールされます。デフォルトでは`~/.roswell/local-projects/`からライブラリが読み込まれます。RoswellのREPLでライブラリを読み込むには、`(ql:quickload :ライブラリ名`)とします。

## .roswell/bin

`ros install <ライブラリ名>`としてライブラリをインストールすると、プロジェクトの`roswell`フォルダにあるRoswell Scriptが`~/.roswell/bin`にコピーされます。`~/.bashrc`等で次のようにPATHを通しておくことで、`~/.roswell/bin`内にあるRoswell Scriptをターミナルのコマンドとして使うことができます。

```
export PATH=$PATH:~/.roswell/bin
```

roveのプロジェクトでは、プロジェクト・トップの`roswell`フォルダの中に`rove.ros`が入っています。`ros install fukamachi/rove`とすることで、`rove.ros`が`~/.roswell/bin/rove`にコピーされます。インストール後は`rove`コマンドが使えるようになります。

```
$ rove
Usage: rove [option...] test-file

Options:
    -r, --reporter REPORTER-NAME    Reporter style (one of "spec", "dot", or "none")
```

### ライブラリの更新

`ros update <ライブラリ名>`とすることで、ライブラリをGitHub上の最新版に更新することができます。
 
```
$ ros update rove
```

## まとめ

* Roswellで処理系をインストールするには`ros install <処理系>`とする。

* RoswellでREPLを起動するには`ros run`とする。

* Roswell Scriptを用いると、Common Lispでシェルコマンドを書くことができる。

* ros buildでは、Roswell Scriptをビルドして実行ファイルを生成することができる。

* ライブラリをGitHubからインストールするには`ros install GitHubアカウント名/レポジトリ名`とする。

* プロジェクト・トップのroswellフォルダにRoswell Scriptを入れておくと、Roswell Scriptをターミナルのコマンドとして使うことができる。

* ライブラリを最新版に更新するには、ros update <ライブラリ名>とする。
