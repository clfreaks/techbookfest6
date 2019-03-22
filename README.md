# techbookfest6

[![CircleCI](https://circleci.com/gh/clfreaks/techbookfest6.svg?style=svg)](https://circleci.com/gh/clfreaks/techbookfest6)

技術書店6で出版するCommon Lisp本

Common Lispの言語について説明している本はけっこうあるが、最近の開発フローについて解説した本はあまりない。そこをカバーする本を作りたい。

- Roswell ( @snmsts @t-cool )
  - Roswellのインストール
  - SBCLのインストール
  - Quicklisp/ASDFについて
  - Ros scriptの書き方 -> ビルド

- Lem ( @cxxxr @t-cool )
  - Lemのインストール ( @t-cool )
  - 基本的な使い方 ( @t-cool )
  - vi-modeについて
  - SLIMEの仕組みと使い方
    - デバッガ、インスペクタ
  - 拡張の書き方( @cxxxr )

- ライブラリの調べ方 ( @t-cool )
  - CLiki ( @t-cool )
  - Quickdocs.org ( @t-cool )
  - Github ( @t-cool )

- プロジェクトの作り方 ( @t-cool )
  - package-inferred-systemについて ( @t-cool )
  - cl-project ( @t-cool )
  - qlot( @t-cool )

- プロジェクト例1: Webスクレイピングとか ( @masatoi ?)
  - プロジェクト生成 -> asdファイルを書く
  - 機械学習でドキュメント分類とか・・・

- プロジェクト例2: チャットボット ( @carrotflakes ?)
  - CFFIなど

- テスト ( @fukamachi )
  - Rove ( @fukamachi )
  
- 配布
  - githubからのインストール
  - Quicklispへの登録申請

- デプロイ ( @gos-k? )
  - docker、heroku、 AWS

## 執筆の流れについて

章ごとにディレクトリを切ったので、その中にMarkdownかorg-modeで書いていってください。
後で @masatoi がAsciidocに変換して調整します。


## ビルド
```
$ docker pull nuitsjp/mdview:2.5
$ cd ~/techbookfest6
$ docker run -v `pwd`:/work -it nuitsjp/mdview:2.5 /bin/bash
# cd /work
# ./make-review.sh
```
これで `techbookfest6/src/book.pdf` ができます。この状態でDockerのシェルを起動させっぱなしにして、mdファイルを編集しては `./make-review.sh` を実行するというサイクルを回すのがいいかと思います。
