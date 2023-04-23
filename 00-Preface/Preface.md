# まえがき

## Lispについて

Lispはもともとは計算モデルの記述法として誕生し、後に実装されプログラミングに使えることが分かったという変わった出自を持つ言語です。
その歴史は古く、最初にジョン・マッカーシーによってそのアイディアが提唱されてからすでに60年以上が経過しています。それにも関わらず、現在においても様々なLisp方言が様々なプラットフォーム上で作られ、使われ続けています。
その理由としては、Lispの基本原理が非常にコンパクトであり実装が容易であること、そして言語の拡張が容易であることが考えられます。こうした特徴から、その時々の最新の言語機能を実験したり、アプリケーション組込みの言語として使われてきました。

Lisp方言のうち、現在でも実際のプロダクトの開発に活用されているものの代表がClojureとCommon Lispです。
ClojureはJVM言語であり、Javaの既存のライブラリ群との相互運用が可能で、幅広いプラットフォーム上で動作するという特徴があります。
Common LispはANSI（米国規格協会）で言語仕様が定められており、ANSI規格に従って書かれたプログラムには可搬性があり、将来にわたって動作することが保証されています。そのような理由から、Common Lispは産業や軍事、宇宙開発、AI研究などの分野で利用されてきました。

本書の著者の中には電子マネープラットフォームを開発しているポケットチェンジ社のメンバーが含まれています。
ここでも決済システムやWebアプリケーションのバックエンドにCommon Lispが活用されています。

## 本書の目的と対象読者

本書はCommon Lispの最近の開発フローやツールの解説、及び具体的なプロジェクト例から構成されています。
Common Lispの言語解説書は質、量ともにそれなりに出揃っている感がありますが、その一方でどのように環境を構築し、プロジェクトを作っていけばいいのかという具体的な指針に欠けていました。
本書はその部分をカバーすることを目的としており、言語そのものの詳細な解説は他書籍などに譲ることとします。

そのため対象読者は、Common Lispに興味があり、言語仕様などは調べたものの（あるいは調べられるものの）、具体的にどのようにプロジェクトを開発していけばいいのか分からないという人や、これからCommon Lispの世界に入っていこうとする人となります。
また、既にCommon Lispを使って開発をしていて最近のツールについて知りたいという人にとっても得るものがあると思われます。

## clfreaksとは

clfreaksは深町英太郎と佐野匡俊を中心に集まったCommon Lisp集団です。不定期でハッカソンや[Podcast](http://clfreaks.org)の配信を行っています。

## 参照してほしい情報源

### 既存のCommon Lispの書籍

前述した通り、本書ではCommon Lispの言語機能自体の解説は目的ではないので、おすすめの解説書をいくつか挙げておきます。本書と併わせて参照してください。

- **実践Common Lisp**: 本格的な言語解説書です。実例も多く、最初の一冊に適しています。
  - 英文の原書は次のURLで無料公開されています([http://www.gigamonkeys.com/book/](http://www.gigamonkeys.com/book/))。
- **実用Common Lisp**: 通称PAIP。古典的なAI開発を通じてCommon Lispを学ぶことができます。
  - 英文の原書は次のURLで無料公開されています([https://github.com/norvig/paip-lisp](https://github.com/norvig/paip-lisp))。
- **Land of Lisp**: テキストベースのゲーム開発などを通じてCommon Lispを学ぶことができます。
- **On Lisp**: Common Lispのマクロを中心に解説した本です。事例も豊富です。
  - 英文の原書は次のURLで無料公開されています([http://www.paulgraham.com/onlisptext.html](http://www.paulgraham.com/onlisptext.html))。

### HyperSpec

[HyperSpec](http://www.lispworks.com/documentation/HyperSpec/Front/)はLispWorksが提供しているANSI Common Lispの仕様書です。ページデザインの古臭さは否めませんが、無料でWeb上で読めるリソースとしては非常に有用です。テキストブラウザでも読めるためエディタ上から参照することもできます。

Common Lispの仕様に困ったときは、Googleで`clhs restart-case`のようにclhs(Common Lisp HyperSpec)と一緒に検索すると目的のページが出てくるので便利です。

### ライブラリの調べ方

Common Lispのライブラリの情報を目的別に調査するためのWebサイトとして、QuickdocsとCLikiがあります。

- [Quickdocs](http://quickdocs.org)は、Common Lisp製ライブラリのドキュメントサイトで、目的別にライブラリが整理されています。
- [CLiki](https://www.cliki.net)は、Common Lisp Foundationが運営しているCommon LispのWikiです。
Current recommended libraries (推薦ライブラリ) のページでは、推薦ライブラリが目的別で確認できます。
