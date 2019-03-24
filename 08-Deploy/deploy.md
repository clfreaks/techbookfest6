# HerokuでCommon Lispを使う

## はじめに

Webサービスを作成する場合、ローカルのコンピュータではなくインターネットからアクセスの出来るサーバでの実行が必要となります。
ここではPaaSの一つであるHeroku(`https://jp.heroku.com`)に対してCommon Lispで書かれたWebサービスをデプロイして実行する方法について説明します。

## 前提条件

Herokuのアカウント作成やコマンドインストールに関しては、言語に関わらず共通のため省略します。

## 使い方

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

## 関連リンク

* https://devcenter.heroku.com/start
* https://dashboard.heroku.com
* https://github.com/gos-k/heroku-buildpack-roswell
