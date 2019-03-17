# techbookfest6
技術書店6で出版するCommon Lisp本

Common Lispの言語について説明している本はけっこうあるが、最近の開発フローについて開発した本はあまりない。そこをカバーする本を作りたい。

- Roswell ( @snmsts @t-cool )
  - Roswellのインストール
  - SBCLのインストール
  - Quicklisp/ASDFについて
  - Ros scriptの書き方 -> ビルド

- Lem ( @cxxxr ? @gos-k ?)
  - Lemのインストール ( @t-cool )
  - 基本的な使い方 ( @t-cool )
    - vi-modeについて
    - SLIMEの仕組みと使い方
      - デバッガ、インスペクタ
  - 拡張の書き方

- ライブラリの調べ方 ( @masatoi ?)
  - CLiki
  - Quickdocs.org / Github

- プロジェクトの作り方 ( @fukamachi ?)
  - package-inferred-systemについて
  - cl-project
  - qlot
    - qlotのインストール

- プロジェクト例1: Webスクレイピングとか ( @masatoi ?)
  - プロジェクト生成 -> asdファイルを書く
  - 機械学習でドキュメント分類とか・・・

- プロジェクト例2: チャットボット ( @carrotflakes ?)
  - CFFIなど

- テスト ( @taniryo ?)

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
docker pull nuitsjp/mdview:2.5
docker run -v `pwd`:/work nuitsjp/mdview:2.5 /bin/sh -c "cd /work && sh ./make-review.sh && cd /src && review-pdfmaker config.yml"

docker run -v /home/wiz/techbookfest6:/work -it nuitsjp/mdview:2.5 /bin/bash
```
