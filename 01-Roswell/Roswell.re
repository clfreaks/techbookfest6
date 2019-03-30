
= 環境構築「Roswell」

本章はRoswellについて解説します。
Roswellは複数のCommon Lispの処理系のインストール、管理、切り替えや、処理系によって異なるコマンドラインオプションの共通化等を行なう処理系マネージャです。
Roswellの使い方はCommon Lisp処理系を使う上での導入になるため、他の章に入る前に説明します。

== Roswellのインストール

=== Homebrewを利用したインストール

LinuxとmacOSではHomebrewでインストールすることができます。

//cmd{
$ brew install roswell
//}

=== Windowsでのインストール

Windows環境でのインストール用にバイナリファイルを用意しています。

 * @<tt>{https://github.com/roswell/roswell/wiki/Installation#windows}

解凍したディレクトリ下で @<tt>{ros shell} を実行すると、長いセットアップ時間の後にmsysが含まれた環境が提供されます。
本書の内容はWindowsに向けて書かれているわけではないので、本書の共としての推奨はしませんが、Windowsネイティブな開発がしたい場合には思い出してもらえると良いかと思います。


=== ソースからインストールする

Homebrewが使えない環境では、Roswellをソースコードからコンパイルすることでインストールできます。
ソースコードからのコンパイルには、以下の環境を揃えたLinux/FreeBSD/macOSが必要です。

 * Cのコンパイル環境
 * libcurlと、そのヘッダ
 * automake/autoconf

Debianベースの環境の場合、インストール手順は次の通りです。

//cmd{
$ if which apt-get > /dev/null; then sudo apt-get -y install git build-essential \
 automake libcurl4-openssl-dev;fi
$ git clone -b release https://github.com/roswell/roswell.git
$ cd roswell
$ sh bootstrap
$ ./configure
$ make
$ sudo make install
$ ros setup
//}

詳細は@<tt>{https://github.com/roswell/roswell/wiki/Installation}で確認できます。

== Common Lisp処理系のインストール


Roswellは、複数のCommon Lisp処理系から特定のバージョンを選んでインストールすることができます。
ここでは、SBCL(Steel Bank Common Lisp)@<fn>{sbcl}をインストールします。

//footnote[sbcl][http://www.sbcl.org/]

各アーキテクチャ向けにビルド済みのバイナリとして配布されている最新のSBCLをインストールするには@<tt>{sbcl-bin}を、ソースコードからビルドするには@<tt>{sbcl}を指定します。

//cmd{
# SBCLのビルド済みのバイナリをインストールする
$ ros install sbcl-bin

# SBCLを最新のソースコードからビルドしてインストールする
$ ros install sbcl
//}

バージョンを指定してインストールするには、スラッシュの後にバージョンを指定します。

//cmd{
# SBCL(1.4.1)のバイナリをインストールする
$ ros install sbcl-bin/1.4.1

# SBCL(1.4.1)をビルドしてインストールする
$ ros install sbcl/1.4.1
//}

@<tt>{ros list installed}で現在インストール済みの処理系を確認できます。
また、@<tt>{ros use <処理系>/<バージョン>}で使用する処理系を切り替えることができます。

//cmd{
# インストール済みの処理系を確認
$ ros list installed

Installed versions of sbcl-bin:
sbcl-bin/1.4.1
sbcl-bin/1.4.16

# 使用する処理系の切り替え
$ ros use sbcl-bin/1.4.1
//}


== REPLの起動

@<tt>{ros run}でCommon Lisp処理系を起動することができます。そうするとREPLが起動し、プロンプトが表示されて入力待ち状態になります。
ここにLisp式を入力することで評価結果が印字されます。

終了するには@<tt>{(quit)}を入力します。

//cmd{
$ ros run
* (princ "hello, world")
hello, world
"hello, world"
* (quit)
$
//}

== Roswell Script

Roswell Scriptを用いると、Common Lispでコマンドラインから利用できるスクリプトを書くことができます。


=== 雛形からRoswell Scriptを作成する

ターミナルで@<tt>{ros init <スクリプト名.ros>}とすると、Roswell Scriptの雛形が生成されます。

//cmd{
$ ros init fact.ros
Successfully generated: fact.ros
//}

生成されたファイルを開き、階乗(factorial)を計算する関数@<code>{fact}を定義してみます。

//emlist[][common-lisp]{
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
//}

=== Roswell Scriptの実行


@<tt>{ros <スクリプト名.ros> 引数}の形式で@<code>{main}関数に引数が渡されて実行されます。

//cmd{
$ ros fact.ros 10
Factorial 10 = 3628800
//}

=== ros build (実行ファイルの作成)

@<tt>{ros build <Roswell Script名>}とすると、指定したRoswell Scriptがビルドされ、実行ファイルが生成されます。

//cmd{
$ ros build fact.ros
compressed 0 bytes into 8 at level -1
compressed 32768 bytes into 401 at level -1
compressed 31719424 bytes into 7068417 at level -1
compressed 2097152 bytes into 562396 at level -1
compressed 12910592 bytes into 3890295 at level -1
//}

ビルド前後の実行時間を比較すると、ビルド後は5倍ほど高速化したことが分かります。

//cmd{
# ビルド前
$ time ros fact.ros 10
Factorial 10 = 3628800

real    0m0.786s
user    0m0.628s
sys 0m0.133s

# ビルド後
$ time fact 10
Factorial 10 = 3628800

real    0m0.187s
user    0m0.156s
sys 0m0.026s
//}

== ライブラリのインストール

@<tt>{ros install}でライブラリのインストールを行います。

Roswellは、Common Lispで利用されるライブラリの集積と依存関係の解決を目的としたQuicklispというソフトウェアを利用し、
コマンドラインからライブラリをインストールするインターフェースを提供します。

@<tt>{ros install <ライブラリ名>}とすると、Quicklispのアーカイブからダウンロードされます。
ここでは、テストの章で利用するroveをインストールしましょう。

//cmd{
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
//}

@<tt>{ros install <GitHubアカウント名/レポジトリ名>}とすると、Quicklispとは別に、GitHubのレポジトリからダウンロードすることもできます。　

ここでroveをGitHubからインストールします。
Quicklispの更新とテストは毎月一度手作業で行なわれている為、同じライブラリを指定した場合でも、githubの方が(更新されていればですが)より新しいバージョンがインストールできます。

//cmd{
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
//}

@<tt>{ros install}でライブラリをインストールすると、ローカル環境の@<tt>{~/.roswell/local-projects/}以下にソースコードがダウンロードされてインストールされます。
デフォルトでは@<tt>{~/.roswell/local-projects/}からライブラリが読み込まれます。
RoswellのREPLでライブラリを読み込むには、@<tt>{(ql:quickload :ライブラリ名)}とします。

== .roswell/bin

@<tt>{ros install <ライブラリ名>}としてライブラリをインストールすると、プロジェクトの@<tt>{roswell}フォルダにあるRoswell Scriptが@<tt>{~/.roswell/bin}にコピーされます。
@<tt>{~/.bashrc}等で次のようにPATHを通しておくことで、@<tt>{~/.roswell/bin}内にあるRoswell Scriptをターミナルのコマンドとして使うことができます。

//emlist{
export PATH=$PATH:~/.roswell/bin
//}

例えばroveでは、プロジェクト直下のの@<tt>{roswell}フォルダの中に@<tt>{rove.ros}が入っています。
@<tt>{ros install fukamachi/rove}とすることで、@<tt>{rove.ros}が@<tt>{~/.roswell/bin/rove}にコピーされます。
インストール後は@<tt>{rove}コマンドが使えるようになります。

//cmd{
$ rove
Usage: rove [option...] test-file

Options:
    -r, --reporter REPORTER-NAME    Reporter style (one of "spec", "dot", or "none")
//}

=== ライブラリの更新

@<tt>{ros update <ライブラリ名>}とすることで、ライブラリをGitHub上の最新版に更新することができます。

//cmd{
$ ros update rove
//}

== まとめ
 * Roswellで処理系をインストールするには@<tt>{ros install <処理系>}とする。
 * RoswellでREPLを起動するには@<tt>{ros run}とする。
 * Roswell Scriptを用いると、Common Lispでシェルコマンドを書くことができる。
 * ros buildでは、Roswell Scriptをビルドして実行ファイルを生成することができる。
 * ライブラリをGitHubからインストールするには@<tt>{ros install GitHubアカウント名/レポジトリ名}とする。
 * プロジェクト・トップのroswellフォルダにRoswell Scriptを入れておくと、Roswell Scriptをターミナルのコマンドとして使うことができる。
 * ライブラリを最新版に更新するには、ros update <ライブラリ名>とする。
