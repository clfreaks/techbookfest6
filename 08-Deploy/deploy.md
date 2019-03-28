# Webアプリの本番環境へのデプロイ

この章では第三章で作った地名検索アプリ「yubin」をWebアプリケーションとして本番環境にデプロイする方法を紹介します。一つ目は仮想コンテナツール「Docker」を使ったデプロイ、もう一つはメジャーなPaaS (Platform as a Service) であるHerokuへのデプロイについて説明します。

## Webアプリの開発

### Clack

Common LispでWebアプリを作るためにはいくつかの方法がありますが、現在ではClackを使うのが主流です。ClackとはさまざまなWebサーバーのインターフェイスを統一し、ソフトウェアのコードを変更することなく複数のWebサーバー上で動かすことができます。たとえば、開発環境ではピュアCommon LispのHunchentootを使い、本番環境ではより高速なWooを使うといったことが可能となります。

このClackをベースとしてWebフレームワークにはningleやCaveman2、Utopianなどがあります。長くなるためここではWebアプリの作り方は紹介しませんが、興味がある方はぜひ調べてみてください。この章では仮に以下のような一ファイルのClackアプリケーションを使います。

```
;; app.lisp
(ql:quickload '(:ningle :cl-mustache :yubin) :silent t)

(defun render (template &optional context)
  (format nil "~
<!doctype html>
<html>
  <head>
    <meta charset='utf-8'>
    <title>yubin</title>
  </head>
  <body>
    ~A
  </body>
</html>"
          (mustache:render* template context)))

(defvar *app* (make-instance 'ningle:app))

(defun root-handler (params)
  (let ((postal-code (cdr (assoc "postal_code" params :test #'equal))))
    (if (stringp postal-code)
        (render "〒{{postal-code}}は「{{place}}」です"
                `(("postal-code" . ,postal-code)
                  ("place" . ,(yubin:get-place postal-code))))
        (render "<form>
                   郵便番号: <input type='text' name='postal_code'>
                   <input type='submit'>
                 </form>"))))

(setf (ningle:route *app* "/") #'root-handler)

*app*
```

このClackアプリを動かすには `clackup` コマンドを使います。 `clackup` コマンドがない場合は `ros install clack` をしてClackをインストールしてください。`clackup` を実行してしばらく待ち `Listening on localhost:5000` と表示されれば起動完了です。ブラウザで http://localhost:5000 を開くと郵便番号の入力フォームが表示されます。

```
# インストール
$ ros install clack
# Webサーバー起動
$ clackup app.lisp
Hunchentoot server is going to start.
Listening on localhost:5000.
```

### Qlot

Webアプリを本番環境にデプロイするときに問題となりうるのが、依存ライブラリのバージョン管理です。開発環境と本番環境で同じバージョンのライブラリを使わなければ環境によって挙動の一貫性を保つことができません。また、複数人で開発する場合にも各人の環境でのバージョン統一が必要となりますし、自分一人の開発であったとしても同じマシンで複数のプロジェクトを扱うときには、同じライブラリでもプロジェクトごとに異なるバージョンを使わなければならないケースがよくあります。

Qlotは、プロジェクトごとにライブラリを管理するためのツールです。Qlotでは依存ライブラリの情報を `qlfile` に記載することでどの環境でも同じバージョンの依存ライブラリ群をインストールすることができます。

まずはいつも通りRoswellでQlotをインストールします。執筆時点のQlotのバージョンは0.9.9です。

```
$ ros install qlot
$ qlot --version
Qlot 0.9.9
```

利用するには `qlfile` を追加します。まずは空の `qlfile` を作り、 `qlot install` で依存ライブラリのセットアップをしましょう。

```
$ touch qlfile
$ qlot install
```

完了すると新しく `qlfile.lock` と `quicklisp/` ディレクトリが作られます。 `qlfile.lock` は `qlfile` を元に必要なライブラリバージョンを解決した情報が含まれているので、必ずリポジトリに含めてください。 `quicklisp/` ディレクトリは依存ライブラリのソースコードがダウンロードされているため、リポジトリに含める必要はありません。以下はgitリポジトリを使う場合の利用例です。

```
$ echo quicklisp/ >> .gitignore
$ git add qlfile qlfile.lock
$ git commit -m 'Start using Qlot.'
```

以降、 `qlot install` をするとどの環境でも同じバージョンの依存ライブラリ群がインストールできます。

QlotではQuicklispだけでなくgitリポジトリを指定してライブラリをインストールすることもできます。以下に `qlfile` の一例を示します。ブランチやタグを指定したり特定のコミットを指定したりもできます。詳しくは[QlotのREADME](https://github.com/fukamachi/qlot)を参照してください。

```
# qlfile例
ql :all 2018-02-28                          # Quicklispの2018-02-28のdistを利用する
ql clack :latest                            # Clackのみ最新の登録バージョンを利用する
git lsx https://github.com/fukamachi/lsx    # LSXはgitリポジトリからダウンロードする
```

Qlotが有効な状態でCommon Lisp環境を起動するにはプロジェクトルートに移動し、実行コマンドの前に `qlot exec` をつけます。たとえばREPLを起動するには `qlot exec ros run` のようにします。LemでSLIMEを起動するときには `C-u M-x slime` を実行して `qlot/sbcl/1.4.8` のように処理系名の前に `qlot/` がついたものを選択します。

QlotではRoswellスクリプトの対応もしています。qlfileによりインストールされる依存ライブラリのRoswellスクリプトは `quicklisp/bin/` の下にインストールされます。たとえば `clack` の場合は `quicklisp/bin/clackup` がインストールされます。このスクリプトの実行に限り `qlot exec` を省略してもQlotが有効な状態で実行されます。

```
# REPLを起動
$ qlot exec ros run
# Roswellスクリプトの実行
$ quicklisp/bin/clackup app.lisp
```

依存ライブラリのバージョンを更新するには `qlot update` が使えます。実行すると `qlfile.lock` の内容が更新されます。

```
# すべての依存ライブラリを更新する (qlfile.lockを作り直す)
$ qlot update
# 特定のライブラリのみ更新する場合は --project を指定する
$ qlot update --project clack
```

## Dockerイメージとしてデプロイする場合

それでは仮想コンテナツールDockerを使ってマシンイメージを作る場合を説明します。Dockerを使えば同じマシンイメージをAWSやGCP、Azureのようなクラウドホスティングサービスにデプロイすることができます。

まずはDockerを利用するためにはDockerfileというファイルを作ります。このファイルはマシンイメージを作るための手順を記述したものです。ベースとなるDockerイメージを `FROM` に指定しています。Roswellが利用可能なDockerイメージは多くの人が独自に作ったものがいくつも乱立している状況で、どれを使うべきかは将来的に変わる可能性があります。ここでは40antsが提供するDockerイメージを使います。

```
FROM 40ants/base-lisp-image:0.6.0-sbcl-bin as base

COPY . /app
RUN qlot install
RUN qlot exec ros build roswell/yubin-server.ros

EXPOSE 5000
ENTRYPOINT ["qlot", "exec"]
CMD ["roswell/yubin-server"]
```

起動を早くするためにDockerイメージの作成時にアプリもビルドしてしまうよう、新たに `roswell/yubin-server.ros` というファイルを追加しています。内容を以下に示します。

```
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp (ql:quickload '(:yubin :clack :clack-handler-woo) :silent t))

(defpackage :ros.script.yubin-server.3762644102
  (:use :cl))
(in-package :ros.script.yubin-server.3762644102)

(defvar *app*
  (clack:eval-file
    (asdf:system-relative-pathname :yubin #P"app.lisp")))

(defun main (&rest argv)
  (declare (ignorable argv))
  (clack:clackup *app*
                 :server :woo
                 :address "0.0.0.0"
                 :port 5000
                 :debug nil
                 :use-thread nil))
;;; vim: set ft=lisp lisp:
```

DockerfileからDockerイメージを作って実行するためには `docker build` と `docker run` を行います。 `Listening on localhost:5000` と表示されたら起動完了です。

```
$ docker build . -t yubin
$ docker run -it -p 5000:5000 yubin
```

実際にクラウドホスティングサービスへデプロイする手順はCommon Lispに限定されないため割愛します。利用したいそれぞれのサービスのドキュメントをご覧ください。

- AWS Elastic Beanstalk
  * https://docs.aws.amazon.com/ja_jp/elasticbeanstalk/latest/dg/single-container-docker.html
- Google Compute Engine
  * https://cloud.google.com/compute/docs/instance-groups/deploying-docker-containers?hl=ja

## Herokuにデプロイする場合

もう一つの例として代表的なPaaSの一つである[Heroku](https://jp.heroku.com)にデプロイする方法について説明します。Herokuのアカウント作成やコマンドインストールに関しては、言語に関わらず共通のため省略します。

### 使い方

Herokuでは標準でCommon Lispをサポートしていないため、カスタムビルドパックとして`heroku-buildpack-roswell`を使用します。

```
$ heroku create --buildpack https://github.com/gos-k/heroku-buildpack-roswell
```

実行するとEmailとPasswordを要求されるので、事前にアカウント作成した時のものを入力してください。

```
heroku-cli: migrating plugins
heroku-cli: done migrating plugins
Creating app... !
 ▸    Invalid credentials provided.
Enter your Heroku credentials:
Email: xxxxxxxxxx@yyyyyyyyyy.com
Password: **********
Creating app... done, ⬢ dry-ridge-44891
Setting buildpack to https://github.com/gos-k/heroku-buildpack-roswell... done
https://dry-ridge-44891.herokuapp.com/ | https://git.heroku.com/dry-ridge-44891.git
```

ここで作成されたアプリケーション名は`dry-ridge-44891`で
(heroku createの度に変わるはずですので、それぞれの環境に合わせて適宜読み替えてください)、
Webサービスが公開されるアドレスが`https://dry-ridge-44891.herokuapp.com`となり、
自分が開発したWebサービスを登録するためのgitリポジトリが`https://git.heroku.com/dry-ridge-44891.git`となります。

```
$ git clone https://git.heroku.com/dry-ridge-44891.git sample
```

`sample`ディレクトリが作成されるので、ここにWebサービスを開発します。
今回はWebフレームワークとしてClackを使用し、そのサンプルにある文字列を返すだけのWebサービスを作成します。

最低限必要なファイルは次の4つです。

* `.roswell-install-list`
* `.roswell-load-system-list`
* `app.lisp`
* `Procfile`

`.roswell-install-list`の内容は次の通りです。
これはclackupコマンドを使用するために、対象のパッケージをインストールします。

```
clack
```

`.roswell-load-system-list`の内容は次の通りです。
これは対象のパッケージをロードを行いキャッシュファイルを生成します。
ここで必要なパッケージが指定されていない場合、起動時にコンパイルが発生した結果、タイムアウトエラーとなる場合があります。

```
clack
```

`app.lisp`の内容は次の通りです。
ここにはclackで実行される文字列を返す関数を定義します。

```
(lambda (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("Hello, Clack!")))
```

`Procfile`の内容は次の通りです。
ここにはサービス起動時に実行されるclackupコマンドを記述します。
`$PORT`はHeroku側から渡されるポート番号で、Heroku内部での通信に使用されます。

```
web clackup --port $PORT app.lisp
```

これらのファイルを追加およびプッシュすると、コンパイルが行われサービスがデプロイされます。

```
git add .roswell-install-list .roswell-load-system-list app.lisp Procfile
git commit -m "Initial commit"
git push
```

初回は処理系やQuicklispのダウンロードも行われるので、プッシュに伴うHeroku側でのリモート実行により、終わるまでに数分を要します。

```
Counting objects: 5, done.
Delta compression using up to 4 threads.
Compressing objects: 100% (3/3), done.
Writing objects: 100% (5/5), 449 bytes | 449.00 KiB/s, done.
Total 5 (delta 0), reused 0 (delta 0)
remote: Compressing source files... done.
remote: Building source:
remote: 
remote: -----> https://github.com/gos-k/heroku-buildpack-roswell app detected
remote: Build roswell
remote: Cloning into 'roswell'...

... (中略) ...

remote: -----> Discovering process types
remote:        Procfile declares types -> web
remote: 
remote: -----> Compressing...
remote:        Done: 63M
remote: -----> Launching...
remote:        Released v3
remote:        https://dry-ridge-44891.herokuapp.com/ deployed to Heroku
remote: 
remote: Verifying deploy... done.
To https://git.heroku.com/dry-ridge-44891.git
 * [new branch]      master -> master
```

`deployed to Heroku`にあるアドレスからWebサービスにアクセスできます。
(この例では`https://dry-ridge-44891.herokuapp.com`)
Webブラウザでアクセスし`Hello Clack!`が表示されればデプロイ成功です。

動作しなかった場合には、`heroku logs --tail`とするとHeroku側のログを見る事が出来ます。

### 関連リンク

* https://devcenter.heroku.com/start
* https://dashboard.heroku.com
* https://github.com/gos-k/heroku-buildpack-roswell
