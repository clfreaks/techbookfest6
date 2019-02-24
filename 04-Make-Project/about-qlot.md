## Qlot

　Qlotは、プロジェクトで用いる依存ライブラリのバージョンを固定するツールである。Quicklispでは月別のバージョンを指定できるが、Qlotではより詳細にバージョンを指定することができる。qlotファイルに指定の形式でバージョンを記載することで、プロジェクト内のquicklispフォルダにソースコードをダウンロードできる。

 RoswellでQlotをインストールしよう。`ros install`でインストールすると、`qlot`コマンドが使えるようになる。
```
$ ros install fukamachi/qlot
Installing from github fukamachi/qlot
(省略)
Found 1 scripts: qlot
/Users/noguchihiroki/.roswell/bin/qlot
up to date. stop
```

では、qlotファイルでライブラリのバージョンを指定して、`qlot`コマンドでライブラリをインストールしてみよう。

```
git clack https://github.com/fukamachi/clack.git
github datafly fukamachi/datafly :branch v0.7.x
ql log4cl 2014-03-17
```

```
$ qlot install
```

　RoswellのREPLからプロジェクトを起動すると`~/.roswell/local-projects`から依存ライブラリを読み込むが、`qlot exec ros -S . run`とすることで、プロジェクト内のquicklispからライブラリが読み込まれてREPLが起動する。

```
$ qlot exec ros -S . run
*
```

## まとめ

* Qlotを使うとプロジェクトごとに依存ライブラリを固定できる。プロジェクト・ローカルのライブラリを読み込んでプロジェクトを起動するには、`qlot exec ros -S . run`でREPLを起動する。
