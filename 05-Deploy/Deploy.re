
= Webアプリの本番環境へのデプロイ


この章では第三章で作った地名検索アプリ「yubin」をWebアプリケーションとして本番環境にデプロイする方法を紹介します。一つ目は仮想コンテナツール「Docker」を使ったデプロイ、もう一つはメジャーなPaaS (Platform as a Service) であるHerokuへのデプロイについて説明します。


== Webアプリの開発

=== Clack


Common LispでWebアプリを作るためにはいくつかの方法がありますが、現在ではClackを使うのが主流です。ClackとはさまざまなWebサーバーのインターフェイスを統一し、ソフトウェアのコードを変更することなく複数のWebサーバー上で動かすことができます。たとえば、開発環境ではピュアCommon LispのHunchentootを使い、本番環境ではより高速なWooを使うといったことが可能となります。



このClackをベースとしてWebフレームワークにはningleやCaveman2、Utopianなどがあります。長くなるためここではWebアプリの作り方は紹介しませんが、興味がある方はぜひ調べてみてください。この章では仮に以下のような一ファイルのClackアプリケーションを使います。


//emlist{
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
//}


このClackアプリを動かすには @<tt>{clackup} コマンドを使います。 @<tt>{clackup} コマンドがない場合は @<tt>{ros install clack} をしてClackをインストールしてください。@<tt>{clackup} を実行してしばらく待ち @<tt>{Listening on localhost:5000} と表示されれば起動完了です。ブラウザで http://localhost:5000 を開くと郵便番号の入力フォームが表示されます。


//emlist{
# インストール
$ ros install clack
# Webサーバー起動
$ clackup app.lisp
Hunchentoot server is going to start.
Listening on localhost:5000.
//}

=== Qlot


Webアプリを本番環境にデプロイするときに問題となりうるのが、依存ライブラリのバージョン管理です。開発環境と本番環境で同じバージョンのライブラリを使わなければ環境によって挙動の一貫性を保つことができません。また、複数人で開発する場合にも各人の環境でのバージョン統一が必要となりますし、自分一人の開発であったとしても同じマシンで複数のプロジェクトを扱うときには、同じライブラリでもプロジェクトごとに異なるバージョンを使わなければならないケースがよくあります。



Qlotは、プロジェクトごとにライブラリを管理するためのツールです。Qlotでは依存ライブラリの情報を @<tt>{qlfile} に記載することでどの環境でも同じバージョンの依存ライブラリ群をインストールすることができます。



まずはいつも通りRoswellでQlotをインストールします。執筆時点のQlotのバージョンは0.9.9です。


//emlist{
$ ros install qlot
$ qlot --version
Qlot 0.9.9
//}


利用するには @<tt>{qlfile} を追加します。まずは空の @<tt>{qlfile} を作り、 @<tt>{qlot install} で依存ライブラリのセットアップをしましょう。


//emlist{
$ touch qlfile
$ qlot install
//}


完了すると新しく @<tt>{qlfile.lock} と @<tt>{quicklisp/} ディレクトリが作られます。 @<tt>{qlfile.lock} は @<tt>{qlfile} を元に必要なライブラリバージョンを解決した情報が含まれているので、必ずリポジトリに含めてください。 @<tt>{quicklisp/} ディレクトリは依存ライブラリのソースコードがダウンロードされているため、リポジトリに含める必要はありません。以下はgitリポジトリを使う場合の利用例です。


//emlist{
$ echo quicklisp/ >> .gitignore
$ git add qlfile qlfile.lock
$ git commit -m 'Start using Qlot.'
//}


以降、 @<tt>{qlot install} をするとどの環境でも同じバージョンの依存ライブラリ群がインストールできます。



QlotではQuicklispだけでなくgitリポジトリを指定してライブラリをインストールすることもできます。以下に @<tt>{qlfile} の一例を示します。ブランチやタグを指定したり特定のコミットを指定したりもできます。詳しくはQlotのREADME@<fn>{e3ea61d554c754aac0afb844664b4c37}を参照してください。


//emlist{
# qlfile例
ql :all 2018-02-28                          # Quicklispの2018-02-28のdistを利用する
ql clack :latest                            # Clackのみ最新の登録バージョンを利用する
git lsx https://github.com/fukamachi/lsx    # LSXはgitリポジトリからダウンロードする
//}


Qlotが有効な状態でCommon Lisp環境を起動するにはプロジェクトルートに移動し、実行コマンドの前に @<tt>{qlot exec} をつけます。たとえばREPLを起動するには @<tt>{qlot exec ros run} のようにします。LemでSLIMEを起動するときには @<tt>{C-u M-x slime} を実行して @<tt>{qlot/sbcl/1.4.8} のように処理系名の前に @<tt>{qlot/} がついたものを選択します。



QlotではRoswellスクリプトの対応もしています。qlfileによりインストールされる依存ライブラリのRoswellスクリプトは @<tt>{quicklisp/bin/} の下にインストールされます。たとえば @<tt>{clack} の場合は @<tt>{quicklisp/bin/clackup} がインストールされます。このスクリプトの実行に限り @<tt>{qlot exec} を省略してもQlotが有効な状態で実行されます。


//emlist{
# REPLを起動
$ qlot exec ros run
# Roswellスクリプトの実行
$ quicklisp/bin/clackup app.lisp
//}


依存ライブラリのバージョンを更新するには @<tt>{qlot update} が使えます。実行すると @<tt>{qlfile.lock} の内容が更新されます。


//emlist{
# すべての依存ライブラリを更新する (qlfile.lockを作り直す)
$ qlot update
# 特定のライブラリのみ更新する場合は --project を指定する
$ qlot update --project clack
//}

== Dockerイメージとしてデプロイする場合


それでは仮想コンテナツールDockerを使ってマシンイメージを作る場合を説明します。Dockerを使えば同じマシンイメージをAWSやGCP、Azureのようなクラウドホスティングサービスにデプロイすることができます。



まずはDockerを利用するためにはDockerfileというファイルを作ります。このファイルはマシンイメージを作るための手順を記述したものです。ベースとなるDockerイメージを @<tt>{FROM} に指定しています。Roswellが利用可能なDockerイメージは多くの人が独自に作ったものがいくつも乱立している状況で、どれを使うべきかは将来的に変わる可能性があります。ここでは40antsが提供するDockerイメージを使います。


//emlist{
FROM 40ants/base-lisp-image:0.6.0-sbcl-bin as base

COPY . /app
RUN qlot install
RUN qlot exec ros build roswell/yubin-server.ros

EXPOSE 5000
ENTRYPOINT ["qlot", "exec"]
CMD ["roswell/yubin-server"]
//}


起動を早くするためにDockerイメージの作成時にアプリもビルドしてしまうよう、新たに @<tt>{roswell/yubin-server.ros} というファイルを追加しています。内容を以下に示します。


//emlist{
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
//}


DockerfileからDockerイメージを作って実行するためには @<tt>{docker build} と @<tt>{docker run} を行います。 @<tt>{Listening on localhost:5000} と表示されたら起動完了です。


//emlist{
$ docker build . -t yubin
$ docker run -it -p 5000:5000 yubin
//}


実際にクラウドホスティングサービスへデプロイする手順はCommon Lispに限定されないため割愛します。利用したいそれぞれのサービスのドキュメントをご覧ください。

 * AWS Elastic Beanstalk
 ** https://docs.aws.amazon.com/ja_jp/elasticbeanstalk/latest/dg/single-container-docker.html
 * Google Compute Engine
 ** https://cloud.google.com/compute/docs/instance-groups/deploying-docker-containers?hl=ja


== Herokuにデプロイする場合


もう一つの例として代表的なPaaSの一つであるHeroku@<fn>{45152b661534ef52c557094b671f9876}にデプロイする方法について説明します。Herokuのアカウント作成やコマンドインストールに関しては、言語に関わらず共通のため省略します。


=== 使い方


Herokuでは標準でCommon Lispをサポートしていないため、カスタムビルドパックとして@<tt>{heroku-buildpack-roswell}を使用します。


//emlist{
$ heroku create --buildpack https://github.com/gos-k/heroku-buildpack-roswell
//}


実行するとEmailとPasswordを要求されるので、事前にアカウント作成した時のものを入力してください。


//emlist{
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
//}


ここで作成されたアプリケーション名は@<tt>{dry-ridge-44891}で
(heroku createの度に変わるはずですので、それぞれの環境に合わせて適宜読み替えてください)、
Webサービスが公開されるアドレスが@<tt>{https://dry-ridge-44891.herokuapp.com}となり、
自分が開発したWebサービスを登録するためのgitリポジトリが@<tt>{https://git.heroku.com/dry-ridge-44891.git}となります。


//emlist{
$ git clone https://git.heroku.com/dry-ridge-44891.git sample
//}


@<tt>{sample}ディレクトリが作成されるので、ここにWebサービスを開発します。
今回はWebフレームワークとしてClackを使用し、そのサンプルにある文字列を返すだけのWebサービスを作成します。



最低限必要なファイルは次の4つです。

 * @<tt>{.roswell-install-list}
 * @<tt>{.roswell-load-system-list}
 * @<tt>{app.lisp}
 * @<tt>{Procfile}



@<tt>{.roswell-install-list}の内容は次の通りです。
これはclackupコマンドを使用するために、対象のパッケージをインストールします。


//emlist{
clack
//}


@<tt>{.roswell-load-system-list}の内容は次の通りです。
これは対象のパッケージをロードを行いキャッシュファイルを生成します。
ここで必要なパッケージが指定されていない場合、起動時にコンパイルが発生した結果、タイムアウトエラーとなる場合があります。


//emlist{
clack
//}


@<tt>{app.lisp}の内容は次の通りです。
ここにはclackで実行される文字列を返す関数を定義します。


//emlist{
(lambda (env)
  (declare (ignore env))
  '(200 (:content-type "text/plain") ("Hello, Clack!")))
//}


@<tt>{Procfile}の内容は次の通りです。
ここにはサービス起動時に実行されるclackupコマンドを記述します。
@<tt>{$PORT}はHeroku側から渡されるポート番号で、Heroku内部での通信に使用されます。


//emlist{
web clackup --port $PORT app.lisp
//}


これらのファイルを追加およびプッシュすると、コンパイルが行われサービスがデプロイされます。


//emlist{
git add .roswell-install-list .roswell-load-system-list app.lisp Procfile
git commit -m "Initial commit"
git push
//}


初回は処理系やQuicklispのダウンロードも行われるので、プッシュに伴うHeroku側でのリモート実行により、終わるまでに数分を要します。


//emlist{
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
//}


@<tt>{deployed to Heroku}にあるアドレスからWebサービスにアクセスできます。
(この例では@<tt>{https://dry-ridge-44891.herokuapp.com})
Webブラウザでアクセスし@<tt>{Hello Clack!}が表示されればデプロイ成功です。



動作しなかった場合には、@<tt>{heroku logs --tail}とするとHeroku側のログを見る事が出来ます。


=== 関連リンク
 * https://devcenter.heroku.com/start
 * https://dashboard.heroku.com
 * https://github.com/gos-k/heroku-buildpack-roswell


//footnote[e3ea61d554c754aac0afb844664b4c37][https://github.com/fukamachi/qlot]

//footnote[45152b661534ef52c557094b671f9876][https://jp.heroku.com]
