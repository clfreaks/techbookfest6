= Webアプリの本番環境へのデプロイ

この章では第3章で作った地名検索アプリ「yubin」をWebアプリケーションとして本番環境にデプロイする方法を紹介します。一つ目は仮想コンテナツール「Docker」を使ったデプロイ、もう一つはメジャーなPaaS (Platform as a Service) であるHerokuへのデプロイについて説明します。

== Webアプリの開発

=== ClackベースのWebアプリ

Common LispでWebアプリを作るためにはいくつかの方法がありますが、現在ではClackを使うのが主流です。ClackとはさまざまなWebサーバーのインターフェイスを統一し、ソフトウェアのコードを変更することなく複数のWebサーバー上で動かすことができます。たとえば、開発環境ではピュアCommon LispのHunchentootを使い、本番環境ではより高速なWooを使うといったことが可能となります。

このClackをベースとしてWebフレームワークにはningleやCaveman2、Utopianなどがあります。長くなるためここではWebアプリの作り方は紹介しませんが、興味がある方はぜひ調べてみてください。この章では@<list>{app.lisp}のような一ファイルのClackアプリケーションを使います。

//list[app.lisp][app.lisp][common-lisp]{
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
        (handler-case
            (render "〒{{postal-code}}は「{{place}}」です"
                    `(("postal-code" . ,postal-code)
                      ("place" . ,(yubin:get-place postal-code))))
          (error (e)
            (render (princ-to-string e))))
        (render "<form>
                   郵便番号: <input type='text' name='postal_code'>
                   <input type='submit'>
                 </form>"))))

(setf (ningle:route *app* "/") #'root-handler)

*app*
//}

このClackアプリを動かすには「@<tt>{clackup}」コマンドを使います。 @<tt>{clackup} コマンドがない場合は @<tt>{ros install clack} をしてClackをインストールしてください。@<tt>{clackup} を実行してしばらく待ち @<tt>{Listening on localhost:5000} と表示されれば起動完了です。ブラウザで @<tt>{http://localhost:5000} を開くと郵便番号の入力フォームが表示されます。

//cmd{
# インストール
$ @<b>{ros install clack}
# Webサーバー起動
$ @<b>{clackup app.lisp}
Hunchentoot server is going to start.
Listening on localhost:5000.
//}

=== Qlotでの依存ライブラリ管理

Webアプリを本番環境にデプロイするときに問題となるのが、依存ライブラリのバージョン管理です。開発環境と本番環境で同じバージョンのライブラリを使わなければ環境によって挙動の一貫性を保つことができません。複数人で開発する場合にも各人の環境でのバージョン統一が必要となりますし、自分一人の開発であったとしても同じマシンで複数のプロジェクトを扱うときにはプロジェクトごとに異なるバージョンを使うケースがよくあります。

Qlotは、プロジェクトごとにライブラリを管理するためのツールです。依存ライブラリの情報を @<tt>{qlfile} に記載することでどの環境でも同じバージョンの依存ライブラリ群をインストールすることができます。

===[column] QlotとQuicklispの関係

Quicklispはライブラリのダウンロードとインストールを行うツールとして広く使われていますが、ユーザー単位でインストールされるのですべてのプロジェクトで共有されてしまいます。Qlotでは、このQuicklispをプロジェクトのディレクトリに個別にインストールして、それを切り替える仕組みとなっています。そういう意味では依存ライブラリの管理というより複数のQuicklispディレクトリの管理ツールと言えるかもしれません。

===[/column]

まずはいつも通りRoswellでQlotをインストールします。執筆時点のQlotのバージョンは0.9.9です。

//cmd{
$ @<b>{ros install qlot}
$ @<b>{qlot --version}
Qlot 0.9.9
//}

利用するには @<tt>{qlfile} を追加します。まずは空の @<tt>{qlfile} を作り、 @<tt>{qlot install} で依存ライブラリのセットアップをしましょう。

//cmd{
$ @<b>{touch qlfile}
$ @<b>{qlot install}
//}

完了すると新しく @<tt>{qlfile.lock} と @<tt>{quicklisp/} ディレクトリが作られます。 @<tt>{qlfile.lock} は @<tt>{qlfile} を元に必要なライブラリバージョンを解決した情報が含まれているので、必ずリポジトリに含めてください。 @<tt>{quicklisp/} ディレクトリは依存ライブラリのソースコードがダウンロードされているため、リポジトリに含める必要はありません。以下はgitリポジトリを使う場合の利用例です。

//cmd{
$ @<b>{echo quicklisp/ >> .gitignore}
$ @<b>{git add qlfile qlfile.lock}
$ @<b>{git commit -m 'Start using Qlot.'}
//}

以降、 @<tt>{qlot install} をするとどの環境でも同じバージョンの依存ライブラリ群がインストールできます。

QlotではQuicklispだけでなくgitリポジトリを指定してライブラリをインストールすることもできます。@<list>{qlfile-example}に @<tt>{qlfile} の一例を示します。ブランチやタグを指定したり特定のコミットを指定したりもできます。詳しくはQlotのREADME@<fn>{qlot-readme}を参照してください。

//list[qlfile-example][qlfile例][]{
ql :all 2018-02-28                          # Quicklispの2018-02-28のdistを利用する
ql clack :latest                            # Clackのみ最新の登録バージョンを利用する
git lsx https://github.com/fukamachi/lsx    # LSXはgitリポジトリからダウンロードする
//}

Qlotが有効な状態でCommon Lisp環境を起動するにはプロジェクトルートに移動し、実行コマンドの前に @<tt>{qlot exec} をつけます。たとえばREPLを起動するには @<tt>{qlot exec ros run} のようにします。LemでSLIMEを起動するときには @<tt>{C-u M-x slime} を実行して @<tt>{qlot/sbcl-bin/1.4.8} のように処理系名の前に @<tt>{qlot/} がついたものを選択します。

QlotではRoswellスクリプトの対応もしています。qlfileによりインストールされる依存ライブラリのRoswellスクリプトは @<tt>{.qlot/bin/} の下にインストールされます。たとえば @<tt>{clack} の場合は @<tt>{.qlot/bin/clackup} がインストールされます。このスクリプトの実行に限り @<tt>{qlot exec} を省略してもQlotが有効な状態で実行されます。

//cmd{
# REPLを起動
$ @<b>{qlot exec ros run}
# Roswellスクリプトの実行
$ @<b>{.qlot/bin/clackup app.lisp}
//}


依存ライブラリのバージョンを更新するには @<tt>{qlot update} が使えます。実行すると @<tt>{qlfile.lock} の内容が更新されます。

//cmd{
# すべての依存ライブラリを更新する (qlfile.lockを作り直す)
$ @<b>{qlot update}
# 特定のライブラリのみ更新する場合は --project を指定する
$ @<b>{qlot update --project clack}
//}

== Dockerイメージとしてデプロイする場合

それでは仮想コンテナツールDockerを使ってマシンイメージを作る場合を説明します。Dockerを使えば同じマシンイメージをAWSやGCP、Azureのようなクラウドホスティングサービスにデプロイすることができます。

=== Dockerfileを書く

まずはDockerを利用するためにはDockerfileというファイルを作ります(@<list>{dockerfile-example})。このファイルはマシンイメージを作るための手順を記述したものです。ベースとなるDockerイメージを @<tt>{FROM} に指定しています。Roswellが利用可能なDockerイメージは多くの人が独自に作ったものがいくつも乱立している状況で、どれを使うべきかは将来的に変わる可能性があります。ここでは40antsが提供するDockerイメージ@<fn>{40ants-docker-image}を使います。

//list[dockerfile-example][Dockerfile][]{
FROM 40ants/base-lisp-image:0.6.0-sbcl-bin as base

COPY . /app
RUN qlot install
RUN qlot exec ros build roswell/yubin-server.ros

EXPOSE 5000
ENTRYPOINT ["qlot", "exec"]
CMD ["roswell/yubin-server"]
//}

起動を早くするためにDockerイメージの作成時にアプリもビルドしてしまうよう、新たに @<tt>{roswell/yubin-server.ros} というファイルを追加しています。内容を@<list>{roswell/yubin-server.ros}に示します。

//list[roswell/yubin-server.ros][roswell/yubin-server.ros][common-lisp]{
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

これらのファイルをyubinのリポジトリに作ります。このDockerfileからDockerイメージを作るにはDockerfileがあるディレクトリ――ここではリポジトリルート――で @<tt>{docker build} を行います。コンパイルがあるためやや時間がかかります。しばらく待ちプロセスが終了したら準備完了です。@<tt>{docker run} を行うとDockerイメージを起動できます。

//cmd{
$ @<b>{docker build . -t yubin}
$ @<b>{docker run -it -p 5000:5000 yubin}
//}

@<tt>{Listening on localhost:5000} と表示されたら起動完了です。

　

実際にクラウドホスティングサービスへデプロイする手順はCommon Lispに限定されないため割愛します。利用したいそれぞれのサービスのドキュメントをご覧ください。

 * AWS Elastic Beanstalk：@<br>{}https://docs.aws.amazon.com/ja_jp/elasticbeanstalk/latest/dg/single-container-docker.html
 * Google Compute Engine：@<br>{}https://cloud.google.com/compute/docs/instance-groups/deploying-docker-containers?hl=ja



== Herokuにデプロイする場合

もう一つの例として代表的なPaaSの一つであるHeroku@<fn>{45152b661534ef52c557094b671f9876}にデプロイする方法について説明します。
Herokuのアカウント作成@<fn>{heroku-signup}やコマンドインストール@<fn>{heroku-cli}に関しては、言語に関わらず共通のため省略します。

=== 使い方

Herokuでは標準でCommon Lispをサポートしていないため、カスタムビルドパックとして@<tt>{heroku-buildpack-roswell}@<fn>{git-buildpack-roswell}を使用します。

//cmd{
$ @<b>{heroku create --buildpack https://github.com/gos-k/heroku-buildpack-roswell}
//}

実行するとEmailとPasswordを要求されるので、事前にアカウント作成した時のものを入力してください。

//image[05-heroku-create][heroku createの実行結果例]{
//}

ここで作成されたアプリケーション名は@<tt>{glacial-temple-78277}で
Webサービスが公開されるアドレスが@<tt>{https://glacial-temple-78277.herokuapp.com}となり、
自分が開発したWebサービスを登録するためのgitリポジトリが@<tt>{https://git.heroku.com/glacial-temple-78277.git}となります。

これらはheroku createの度に変わりますので、それぞれの環境に合わせて適宜読み替えてください。

//cmd{
$ @<b>{git clone https://git.heroku.com/glacial-temple-78277.git sample}
Cloning into 'sample'...
warning: You appear to have cloned an empty repository.
$ @<b>{cd sample}
//}

@<tt>{sample}ディレクトリが作成されるので、ここにWebサービスを開発します。
今回はWebフレームワークとしてClackを使用し、そのサンプルにある文字列を返すだけのWebサービスを作成します。

まず、Heroku側でUTF-8を扱うため、環境変数 @<tt>{LANG} を設定します。


//cmd{
$ @<b>{heroku config:set LANG=ja_JP.UTF-8}
//}


最低限必要なファイルは次の4つです。

 * @<tt>{.roswell-install-list}
 * @<tt>{.roswell-load-system-list}
 * @<tt>{app.lisp}
 * @<tt>{Procfile}

@<tt>{.roswell-install-list}の内容は次の通りです。
これは @<tt>{clackup} コマンドを使用するために、対象のパッケージをインストールします。

//emlist{
clack
clfreaks/yubin
//}

@<tt>{.roswell-load-system-list}の内容は次の通りです。
これは対象のパッケージをロードを行いキャッシュファイルを生成します。
ここで必要なパッケージが指定されていない場合、起動時にコンパイルが発生した結果、タイムアウトエラーとなる場合があります。

//emlist{
clack
ningle
cl-mustache
yubin
//}

@<tt>{app.lisp}の内容は、先ほどの物と同様です。

@<tt>{Procfile}の内容は次の通りです。
ここにはサービス起動時に実行されるclackupコマンドを記述します。
@<tt>{$PORT}はHeroku側から渡されるポート番号で、Heroku内部での通信に使用されます。

//emlist{
web clackup --port $PORT app.lisp
//}

これらのファイルを追加およびプッシュすると、コンパイルが行われサービスがデプロイされます。

//cmd{
$ @<b>{git add .roswell-install-list .roswell-load-system-list app.lisp Procfile}
$ @<b>{git commit -m "Initial commit"}
$ @<b>{git push}
//}

初回は処理系やQuicklispのダウンロードも行われるので、プッシュに伴うHeroku側でのリモート実行により、終わるまでに数分を要します。

//cmd{
Counting objects: 3, done.
Delta compression using up to 4 threads.
Compressing objects: 100% (3/3), done.
Writing objects: 100% (3/3), 276 bytes | 276.00 KiB/s, done.
Total 3 (delta 2), reused 0 (delta 0)
remote: Compressing source files... done.
remote: Building source:

... (中略) ...

remote: -----> Discovering process types
remote:        Procfile declares types -> web
remote: 
remote: -----> Compressing...
remote:        Done: 66.6M
remote: -----> Launching...
remote:        Released v6
remote:        https://glacial-temple-78277.herokuapp.com/ deployed to Heroku
remote: 
remote: Verifying deploy... done.
To https://git.heroku.com/glacial-temple-78277.git
   9b50d39..2adaacd  master -> master
//}


@<tt>{deployed to Heroku}にあるアドレスからWebサービスにアクセスできます。
(この例では@<tt>{https://glacial-temple-78277.herokuapp.com/})
Webブラウザでアクセスし、郵便番号を入力するページが表示されればデプロイ成功です。

動作しなかった場合には、@<tt>{heroku logs --tail}とするとHeroku側のログを見る事が出来ます。



//footnote[heroku-signup][https://signup.heroku.com]

//footnote[heroku-cli][https://devcenter.heroku.com/articles/heroku-cli]

//footnote[git-buildpack-roswell][https://github.com/gos-k/heroku-buildpack-roswell]

//footnote[qlot-readme][https://github.com/fukamachi/qlot]

//footnote[40ants-docker-image][https://github.com/40ants/base-lisp-image]

//footnote[45152b661534ef52c557094b671f9876][https://jp.heroku.com]
