
= まえがき

== Lispについて

Lispは元々は計算モデルの記述法として誕生し、後に実装されプログラミングに使えることが分かったという変わった出自を持つ言語です。
その歴史は古く、最初にジョン・マッカーシーによってそのアイディアが提唱されてからすでに60年以上が経とうとしています。それにも関わらず、現在においても様々なLisp方言が様々なプラットフォーム上で作られ、使われ続けています。
その理由としては、Lispの基本原理が非常にコンパクトであり実装が容易であること、そして言語の拡張が容易であることが考えられます。こうした特徴から、その時々の最新の言語機能を実験したり、アプリケーション組込みの言語として使われてきました。

Lisp方言のうち、現在でも実際のプロダクトの開発に活用されているものの代表がClojureとCommon Lispです。
ClojureはJVM言語であり、Javaの既存のライブラリ群との相互運用が可能で、幅広いプラットフォーム上で動作するという特徴があります。
Common LispはANSI(米国規格協会)で言語仕様が定められており、ANSI規格に従って書かれたプログラムには可搬性があり、将来に渡って動くことが保証されます。そのような理由から、Common Lispは産業や軍事、AI研究などの分野で利用されてきました。

本書の執筆陣は電子マネープラットフォームを開発しているポケットチェンジ社のメンバーが中心となっています。
ここでも決済システムやWebアプリケーションのバックエンドにCommon Lispが活用されています。

== 本書の目的と対象読者

本書はCommon Lispの最近の開発フローやツールの解説、及び具体的なプロジェクト例から構成されています。
Common Lispの言語解説書は質、量ともにそれなりに出揃っている感がありますが、その一方でどのように環境を構築し、プロジェクトを作っていけばいいのかという具体的な指針に欠けていました。
本書はその部分をカバーすることを目的としており、言語そのもの詳細な解説はここで挙げているような情報源を参照してください。

そのため対象読者は、Common Lispに興味があり、言語仕様などは調べたが(あるいは調べられるが)、具体的にどのようにプロジェクトを開発していけばいいのか分からないという人や、これからCommon Lispの世界に入っていこうとする人です。
また、既にCommon Lispを使って開発をしているが、最近のツールについて知りたいという人にとっても得るものがある本になっていればいいと思います。

== 参照してほしいリソース

=== 既存のCommon Lispの書籍

前述した通り、本書ではCommon Lispの文法自体の解説はしないため、おすすめの言語解説書をいくつか挙げておきます。本書と併わせて参照してください。

 * @<b>{実践Common Lisp}: 本格的な言語解説書。実例も多く最初の一冊に適している。原書は無料公開されている。
 * @<b>{実用Common Lisp}: 通称PAIP。古典的なAI開発を通じてCommon Lispを学ぶ。原書は無料公開されている。
 * @<b>{Land of Lisp}: テキストベースのゲーム開発を通じてCommon Lispを学ぶ。
 * @<b>{On Lisp}: Common Lispのマクロを中心に解説した本。事例も豊富。原書は無料公開されている。

=== HyperSpec

HyperSpecはLispWorksが提供しているANSI Common Lispの仕様書です。ページデザインの古臭さは否めませんが、無料でWeb上で読めるリソースとしては非常に有用です。テキストブラウザでも読めるためエディタ上から参照することもできます。

 * @<tt>{http://www.lispworks.com/documentation/HyperSpec/Front/}

Common Lispの仕様に困ったときはGoogleで「clhs restart-case」などで検索すると目的のページが出てくるので便利です。

=== ライブラリの調べ方

QuickdocsとCLikiでは、Common Lisp製のライブラリの情報を目的別に参照することができます。Quickdocsは、Common Lisp製ライブラリのドキュメントサイトです。目的別にライブラリが整理されています。CLikiは、Common Lisp Foundationが運営しているCommon LispのWikiです。Current recommended libraries (推薦ライブラリ) のページでは、推薦ライブラリが目的別で確認できます。

 * @<b>{Quickdocs}
 ** @<tt>{http://quickdocs.org}
 * @<b>{CLiki}
 ** @<tt>{https://www.cliki.net}
