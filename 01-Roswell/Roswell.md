# 環境構築ツール「Roswell」

本章ではCommon Lispの環境構築ツール **[Roswell](https://github.com/roswell/roswell)** について解説します。
Roswellは複数のCommon Lisp処理系のインストール、管理、切り替えや、処理系によって異なるコマンドラインオプションの共通化などを行なう処理系マネージャです。
Roswellの使い方はCommon Lispを使う上での導入になるため、他の章に入る前に説明します。

## Roswellのインストール

Roswellのインストールについての最新情報はRoswellのGithub Wikiにある [インストールガイド](https://github.com/roswell/roswell/wiki/Installation) を参照してください。
以下では、本書執筆時点での主だったインストール方法について解説しています。

### Debian系Linuxでのインストール

Debian系Linuxに対しては、[こちらのページ](https://github.com/roswell/roswell/releases)からdebファイルが配布されています。ここでダウンロードしたファイルに対して以下のコマンドを実行することによってインストールできます。

```
$ sudo dpkg -i roswell_<バージョン>.deb
```

### Homebrewを利用したインストール (Linux / macOS)

LinuxとmacOS環境では、Homebrewが導入済みの場合、以下のコマンドでRoswellをインストールすることができます。

```
$ brew install roswell
```

### ソースコードからのインストール (その他のLinux / FreeBSD / macOSなど)

Homebrewが使えない環境では、Roswellをソースコードから直接コンパイルする方法もあります。
ソースコードからのコンパイルには、以下の環境を備えたLinux / FreeBSD / macOSが必要です。

- Cのコンパイル環境
- libcurlと、そのヘッダ
- automake/autoconf

DebianベースのLinuxの場合、ソースコードからのインストール手順は次のようになります。

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

その他のLinuxディストリビューションでのインストール方法については[インストールガイド](https://github.com/roswell/roswell/wiki/Installation) を参照してください。

### Windowsでのインストール

WindowsではScoop(Windows用のアプリインストーラ、アプリ管理ツール)が導入済みの場合、以下のコマンドでRoswellをインストールすることができます。

```
$ scoop install roswell
```

また、Windows環境でのインストール用にバイナリファイルが用意されています。

- [https://github.com/roswell/roswell/wiki/Installation#windows](https://github.com/roswell/roswell/wiki/Installation#windows)

解凍したディレクトリ下で `ros shell` を実行すると、msysを含んだ環境がインストールされます(このセットアップには長時間かかります)。

### Roswellがインストールされていることを確認する

適切にインストールが完了していれば、`ros`コマンドが実行可能になっているはずです。コマンドプロンプトで単に `ros` と入力するとヘルプが表示されます。
また、 `ros -V` とすることでバージョン情報を表示できます。

```
$ ros -V
roswell 20.06.14.107
build with gcc (Ubuntu 7.5.0-3ubuntu1~18.04) 7.5.0
libcurl=7.58.0
Quicklisp=2020-01-04
Dist=2020-07-15
lispdir='/home/user/etc/roswell/'
homedir='/home/user/.roswell/'
configdir='/home/user/.roswell/'
```

## Roswellを利用したCommon Lisp処理系のインストール

コマンドプロンプトで `ros install <処理系名>` とすることで、処理系を指定してインストールすることができます。

ここでは、**[SBCL(Steel Bank Common Lisp)](http://www.sbcl.org/)** をインストールします。
ビルド済みバイナリとして配布されているSBCLをインストールするには `sbcl-bin` を、ソースコードからビルドするには `sbcl` を指定します。

```
# SBCLのビルド済みバイナリをインストールする
$ ros install sbcl-bin

# SBCLを最新のソースコードからビルドしてインストールする
$ ros install sbcl
```

また、特定のバージョンを指定してインストールすることもできます。
バージョンを指定してインストールするには、 `ros install <処理系>/<バージョン>` のようにスラッシュの後にバージョンを指定します。バージョン省略時には最新のバージョンがインストールされます。

```
# SBCL(1.4.1)のバイナリをインストールする
$ ros install sbcl-bin/1.4.1

# SBCL(1.4.1)をビルドしてインストールする
$ ros install sbcl/1.4.1
```

`ros list installed` で現在インストール済みの処理系を確認できます。
また、`ros use <処理系>/<バージョン>`で使用する処理系を切り替えることができます。

```
# インストール済みの処理系を確認
$ ros list installed

Installed versions of sbcl-bin:
sbcl-bin/1.4.1
sbcl-bin/1.4.16

# 使用する処理系の切り替え
$ ros use sbcl-bin/1.4.1
```

## REPLの起動

コマンドプロンプトから`ros run`と入力することで、`ros use`によってRoswellに設定されたCommon Lisp処理系を起動することができます。そうするとREPLが起動し、プロンプトが表示されて入力待ち状態になります。REPLは Read Eval Print Loop の略で、Lisp式を入力することで評価結果が印字される、対話的なインターフェースです。

Lisp処理系を終了するには`(quit)`を入力します。

```
$ ros run
* (princ "hello, world")
hello, world
"hello, world"
* (quit)
$
```

## Roswell Script

**Roswell Script** を用いると、コマンドラインから利用できるスクリプトをCommon Lispで書くことができます。
各Lisp処理系にもスクリプティング用のモードがありますが、Roswell Scriptとして書いておくことでスクリプトへの引数の与え方などの処理系ごとの違いを吸収できます。また、Roswell Scriptをビルドしてスタンドアロンの実行ファイルを作ることも可能です。

### 雛形からRoswell Scriptを作成する

コマンドプロンプトで`ros init <スクリプト名.ros>`とすると、Roswell Scriptの雛形ファイルが生成されます。

```
$ ros init fact.ros
Successfully generated: fact.ros
```

このファイルを編集し、階乗(factorial)を計算するスクリプトを書いてみましょう。
生成されたファイル`fact.ros`を開くと、何もしないスクリプトができています。
このファイルに対して以下のように、`fact`関数を書き加え、`main`関数を`fact`を呼ぶように修正し、保存します。

```lisp
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

コマンドプロンプトから `ros <スクリプト名.ros> 引数` の形式でコマンドを実行することで、スクリプト中の`main`関数にコマンドで指定した引数が渡され、実行されます。

```
$ ros fact.ros 10
Factorial 10 = 3628800
```

`ros init`で作られたスクリプトには実行可能フラグがついているので、単に `./fact.ros 10` とすることでも実行できます。

### Roswell Scriptの実行ファイルのビルド

`ros build <スクリプト名.ros>`とすると、指定したRoswell Scriptがビルドされ、実行ファイルが生成されます。
実行ファイルのファイル名は、`.ros`拡張子が外れて`<スクリプト名>`になります。

```
$ ros build fact.ros
```

ビルド前後の実行時間を比較すると、ビルド後は大幅に高速化されていることが分かります。

```
# ビルド前
$ time ros fact.ros 10
Factorial 10 = 3628800

0.405 secs

# ビルド後
$ time ./fact 10
Factorial 10 = 3628800

0.142 secs
```

ビルド前のRoswell Scriptは単なるテキストファイルなので軽量ですが、ビルド後の実行ファイルはLispのランタイムを含んでいるため、サイズがかなり大きくなるというデメリットがあります。
SBCLの場合はデフォルトでランタイムイメージが圧縮され、実行ファイルの大きさは約12MBになります。
圧縮を無効化することでファイルサイズの増大と引き換えにさらに起動時間を短縮することもできます。

```
$ ros build fact.ros --disable-compression

0.015 secs

```

この場合のファイルサイズは約43MBになっています。

ros buildのより詳細なオプションについては[こちら](https://github.com/roswell/roswell/wiki/Building-images-and-executables)を参照してください。

## Roswellによるライブラリのインストール

Roswellには、Lisp処理系のインストールだけでなく、ライブラリをインストールする機能もあります。
ライブラリのインストールには、 **Quicklisp** のアーカイブからインストールする方法と、GitHubリポジトリからインストールする方法があります。

### Quicklispからのライブラリのインストール
[Quicklisp](https://www.quicklisp.org/beta/)は、Common Lispで利用されるライブラリの集積と、ライブラリ間の依存関係の解決を目的としたソフトウェアです。

ライブラリのインストールには`ros install`コマンドを使用します。以下のコマンドを実行すると、Quicklispのアーカイブからライブラリがダウンロードされ、ローカル環境にインストールされます。
```
ros install <ライブラリ名>
```
ここでは、第4章で使用するテストフレームワークライブラリ **[Rove](https://github.com/fukamachi/rove)** をインストールしてみましょう。

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

### Githubリポジトリからのライブラリのインストール
Quicklispに登録されていないライブラリでも、Githubにリポジトリが登録されていれば以下のようにインストールすることができます。

```
ros install <GitHubアカウント名>/<リポジトリ名>
```


ここでは、先程インストールしたRoveをGitHubのリポジトリからインストールし直してみます。
Quicklispの更新とテストは毎月一度手作業で行なわれています。そのため、同じライブラリを指定した場合でもGitHubの方がより最新版になります。

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

`ros install` でGitHubからライブラリをインストールすると、ローカル環境の `~/.roswell/local-projects` 以下にソースコードがダウンロードされインストールされます。
デフォルトではこの `~/.roswell/local-projects` から優先的にライブラリが読み込まれます。

インストールしたライブラリをLisp処理系で読み込むには、REPLで `(ql:quickload :<ライブラリ名>)` を評価します。

```
$ ros run
* (ql:quickload :rove)
To load "rove":
  Load 1 ASDF system:
    rove
; Loading "rove"

(:ROVE)
*
```

ロードしたライブラリが実際にどのディレクトリから読み込まれているかを確認するには `(asdf:system-source-directory :rove)` を評価します。

```
* (asdf:system-source-directory :rove)
#P"/home/user/.roswell/local-projects/fukamachi/rove/"
```

今、QuicklispとGithubリポジトリの双方からインストールされたRoveが存在していますが、実際にロードされているのはGithubのRoveであることが分かります。これは `~/.roswell/local-projects` がより優先的に読み込まれるためです。

どのバージョンのライブラリが読み込まれるかをより厳密に管理したいときは、プロジェクト内で依存ライブラリのバージョンを固定できるツール **[Qlot](https://github.com/fukamachi/qlot)** を使います。Qlotについては5章で解説します。

### ライブラリの実行ファイルがインストールされるディレクトリ

`ros install <ライブラリ名>`としてライブラリをインストールすると、ライブラリのソースディレクトリ直下にある`roswell`ディレクトリ内のRoswell Scriptが`~/.roswell/bin` 以下にコピーされます。
このディレクトリを `~/.bashrc` 等で環境変数 `PATH` に設定しておくことで、 `~/.roswell/bin` 内にあるRoswell Scriptをコマンドとして使うことができます。

```bash
export PATH=$PATH:~/.roswell/bin
```

例えばRoveの場合、ソースディレクトリ直下の`roswell`ディレクトリ内に`rove.ros`が入っています。
`ros install fukamachi/rove`とすることで、`rove.ros`が`~/.roswell/bin/rove`にコピーされ、インストール後は`rove`コマンドが使えるようになります。

```
$ rove
Usage: rove [option...] test-file

Options:
    -r, --reporter REPORTER-NAME    Reporter style (one of "spec", "dot", or "none")

$ which rove
/home/user/.roswell/bin/rove
```

### ライブラリの更新

`ros update <ライブラリ名>`とすることで、ライブラリをGitHub上の最新版に更新することができます。

```
$ ros update rove
```

## まとめ
この章では、Common Lispの環境構築ツールとしてのRoswellを紹介しました。

- Roswell自体のインストール方法
- RoswellによるCommon Lisp処理系のインストール方法とREPLの起動方法
- Roswell Scriptの雛形の生成とスクリプトの書き方、実行ファイルのビルドについて
- Roswellを利用したCommon Lispのライブラリのインストール方法
