# 環境構築ツール「Roswell」

本章ではCommon Lispの環境構築ツール **[Roswell](https://github.com/roswell/roswell)** について解説します。
Roswellは複数のCommon Lisp処理系のインストール、管理、切り替えや、処理系によって異なるコマンドラインオプションの共通化などを行なう処理系マネージャです。
Roswellの使い方はCommon Lispを使う上での導入になるため、他の章に入る前に説明します。

## Roswellのインストール

### Debian系Linuxでのインストール

Debian系Linuxではapt経由でのインストールが可能です。以下のコマンドでRoswellをインストールすることができます。

```
$ sudo apt install roswell
```

また、[こちらのページ](https://github.com/roswell/roswell/releases)からdebファイルでも配布されています。ここでダウンロードしたファイルに対して以下のコマンドを実行することによってもインストールできます。

```
$ sudo dpkg -i roswell_<バージョン>.deb
```

### Homebrewを利用したインストール (Linux / macOS)

LinuxとmacOSではHomebrewが導入済みの場合、以下のコマンドでRoswellをインストールすることができます。

```
$ brew install roswell
```

### ソースコードからのインストール (その他のLinux / FreeBSD / macOSなど)

Homebrewが使えない環境では、Roswellをソースコードからコンパイルする方法もあります。
ソースコードからのコンパイルには、以下の環境を備えたLinux/FreeBSD/macOSが必要です。

- Cのコンパイル環境
- libcurlと、そのヘッダ
- automake/autoconf

Debianベースの環境の場合、インストール手順は次の通りです。

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

### Windowsでのインストール

WindowsではScoop(Windows用のアプリインストーラ、アプリ管理ツール)が導入済みの場合、以下のコマンドでRoswellをインストールすることができます。

```
$ scoop install roswell
```

Windows環境でのインストール用にバイナリファイルが用意されています。

- [https://github.com/roswell/roswell/wiki/Installation#windows](https://github.com/roswell/roswell/wiki/Installation#windows)

解凍したディレクトリ下で `ros shell` を実行すると、msysを含んだ環境が提供されます(このセットアップには長時間かかります)。
本書の内容はWindowsに向けて書かれているわけではないので、本書の共としての推奨はしませんが、Windowsネイティブな開発がしたい場合には思い出してもらえると良いかと思います。

Roswellのインストールについての詳細は [https://github.com/roswell/roswell/wiki/Installation](https://github.com/roswell/roswell/wiki/Installation) を参照してください。

### Roswellがインストールされていることを確認する

適切にインストールが完了していれば、`ros`コマンドが実行可能になっているはずです。ターミナル上で単に `ros` と入力するとヘルプが表示されます。
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

ターミナル上で `ros install <処理系>` とすることで、処理系を指定してインストールできます。

ここでは、**[SBCL(Steel Bank Common Lisp)](http://www.sbcl.org/)** をインストールします。
ビルド済みバイナリとして配布されているSBCLをインストールするには `sbcl-bin` を、ソースコードからビルドするには `sbcl` を指定します。

```
# SBCLのビルド済みバイナリをインストールする
$ ros install sbcl-bin

# SBCLを最新のソースコードからビルドしてインストールする
$ ros install sbcl
```

処理系のバージョンを指定してインストールすることもできます。
バージョンを指定してインストールするには、 `ros install <処理系>/<バージョン>` のようにスラッシュの後にバージョンを指定します。

```
# SBCL(1.4.1)のバイナリをインストールする
$ ros install sbcl-bin/1.4.1

# SBCL(1.4.1)をビルドしてインストールする
$ ros install sbcl/1.4.1
```

`ros list installed`で現在インストール済みの処理系を確認できます。
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

`ros run`でCommon Lisp処理系を起動することができます。そうするとREPLが起動し、プロンプトが表示されて入力待ち状態になります。
ここにLisp式を入力することで評価結果が印字されます。

終了するには`(quit)`を入力します。

```
$ ros run
* (princ "hello, world")
hello, world
"hello, world"
* (quit)
$
```

## Roswell Script

**Roswell Script**を用いると、Common Lispでコマンドラインから利用できるスクリプトを書くことができます。
各処理系にもスクリプティング用のモードがありますが、Roswell Scriptとして書いておくことでスクリプトへの引数の与え方などの処理系ごとの違いを吸収することができます。また、Roswell Scriptをビルドしてスタンドアロンの実行ファイルを作ることもできます。

### 雛形からRoswell Scriptを作成する

ターミナル上で`ros init <スクリプト名.ros>`とすると、Roswell Scriptの雛形が生成されます。

```
$ ros init fact.ros
Successfully generated: fact.ros
```

例として、階乗(factorial)を計算するスクリプトを書いてみます。
生成されたファイル`fact.ros`を開くと、何もしないスクリプトができています。ここに以下のように、`fact`関数を書き加え、`main`関数を修正して保存します。

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

`ros <スクリプト名.ros> 引数` の形式でコマンドを実行することで、スクリプト中の`main`関数に指定した引数が渡されて実行されます。

```
$ ros fact.ros 10
Factorial 10 = 3628800
```

`ros init`で作られたスクリプトには実行可能フラグがついているので、単に `./fact.ros 10` でも実行できます。

### Roswell Scriptの実行ファイルのビルド

`ros build <スクリプト名.ros>`とすると、指定したRoswell Scriptがビルドされ、実行ファイルが生成されます。
実行ファイルのファイル名は、`.ros`拡張子が外れ、`<スクリプト名>`になります。

```
$ ros build fact.ros
```

ビルド前後の実行時間を比較すると、ビルド後は大幅に高速化されていることが分かります。

```
# ビルド前
$ time ros fact.ros 10
Factorial 10 = 3628800

real	0m1.476s
user	0m1.447s
sys	0m0.015s

# ビルド後
$ time ./fact 10
Factorial 10 = 3628800

real	0m0.011s
user	0m0.008s
sys	0m0.001s
```

## Roswellによるライブラリのインストール

Roswellには、処理系のインストールだけでなく、ライブラリをインストールする機能もあります。
ライブラリのインストールには、**Quicklisp**のアーカイブからインストールする方法と、GitHubリポジトリからインストールする方法があります。
Quicklispは、Common Lispで利用されるライブラリの集積と依存関係の解決を目的としたソフトウェアです。

ライブラリのインストールには`ros install`コマンドを使用します。
`ros install <ライブラリ名>`とすることで、Quicklispのアーカイブからライブラリがダウンロードされ、ローカル環境にインストールされます。
ここでは、第4章で使用するテストフレームワークライブラリ**Rove**をインストールしてみましょう。

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

`ros install <GitHubアカウント名>/<リポジトリ名>`とすると、Quicklispからではなく、GitHubリポジトリからインストールできます。　

ここでは、先程インストールしたRoveをGitHubからインストールし直してみます。
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

`ros install`でGitHubからライブラリをインストールすると、ローカル環境の `~/.roswell/local-projects` 以下にソースコードがダウンロードされインストールされます。
デフォルトではこの`~/.roswell/local-projects`からライブラリが読み込まれます。

インストールしたライブラリをRoswellのREPLで読み込むには、`(ql:quickload :<ライブラリ名>)`とします。

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

### ライブラリの実行ファイルがインストールされるディレクトリ

`ros install <ライブラリ名>`としてライブラリをインストールすると、ライブラリのソースディレクトリ直下にある`roswell`ディレクトリ内のRoswell Scriptが`~/.roswell/bin` 以下にコピーされます。
このディレクトリを `~/.bashrc` 等で環境変数 `PATH` に設定しておくことで、 `~/.roswell/bin` 内にあるRoswell Scriptをコマンドとして使うことができます。

```bash
export PATH=$PATH:~/.roswell/bin
```

例えばRoveの場合、ソースディレクトリ直下の`roswell`ディレクトリ内に`rove.ros`が入っています。
`ros install fukamachi/rove`とすることで、`rove.ros`が`~/.roswell/bin/rove`にコピーされます。
インストール後は`rove`コマンドが使えるようになります。

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
この章では、Common Lispの環境構築ソフトウェアとしてのRoswellを紹介しました。

- Roswell自体のインストール方法
- RoswellによるCommon Lisp処理系のインストール方法とREPLの起動方法
- Roswell Scriptの雛形の生成とスクリプトの書き方、実行ファイルのビルドについて
- Roswellを利用したCommon Lispのライブラリのインストール方法
