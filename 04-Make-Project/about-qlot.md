## Qlot

　Qlotは、プロジェクトごとにライブラリを管理するためのツールである。Quicklispに登録されているライブラリによっては、開発途中にAPIが変更される可能性がある。Qlotでは、依存ライブラリの情報を`qlfile`に記載することで、プロジェクトフォルダ内の`quicklisp`フォルダ内にライブラリがダウンロードされて、ライブラリのバージョンを固定することができる。

　試しに、qlfileを書いて試してみよう。次のように、Github上のブランチ、Quicklispの月別レポジトリを指定することができる。

```
git clack https://github.com/fukamachi/clack.git
github datafly fukamachi/datafly :branch jojo
ql log4cl 2014-03-17
```

　qlfileがあるディレクトリ内で`qlot install`とすると、プロジェクト内のquicklispフォルダにソースコードがダウンロードされた後、インストールされる。

```
$ qlot install
```

　インストール後、quicklispフォルダを開くと、dists以下にライブラリのソースコード、binフォルダにRowell Scriptを確認できる。
 
　プロジェクト内のquicklispフォルダからライブラリをロードするには`qlot exec`を用いる。`qlot exec ros -S . run`とすれば、REPLが起動する。

```
$ qlot exec ros -S . run
* 
```

　`qlot exec <Roswell Script>`とすると、プロジェクト内の`quicklisp/bin`からRoswell Scriptのコマンドを実行することができる。

```
$ qlot exec clackup app.lisp
```

