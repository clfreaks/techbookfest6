= テストフレームワーク「Rove」

== Common Lispのテストライブラリ

Common Lispには数多くのテストライブラリがあります。長い歴史の中でメンテナンスされていないものも含めると三十近くもあります。その中でも最も多く使われているものがFiveAMとProveです。FiveAMはシンプルなテストライブラリです。Common Lispライブラリで古くから使われており、ファンも多くいます。一方でProveはテスト結果の色付けやコマンドラインインターフェイスなどテスト実行環境周辺にも目を向けて作られたものです。

この章ではProveの後継テストフレームワークであるRoveを紹介します。Roveは現代のCommon LispプログラムのトレンドにフィットするようにProveを再構成したものです。たとえば、ASDF 3.1から取り入れられたpackage-inferred-systemというファイル単位でのプログラム分割機能を使えるように改良されています。また、非同期なプログラムのテストのためのスレッドサポートや、テスト実行前後のフック、一定時間以上テストの実行にかかったものを色付けして表示するなどの機能も含まれます。

初めてCommon Lispプログラムを書く人も、今までProveを使っていた人も、これを機会にRoveを使っていただければと思います。

== セットアップ

まずはREPLでRoveをインストール・ロードしましょう。RoveはQuicklispに登録されているため、他のライブラリ同様に @<code>{ql:quickload} でインストールとロードができます。

//emlist{
(ql:quickload :rove)
//}

この本ではバージョン 0.9.1を前提としています。各環境のRoveのバージョン番号を確認し、古いバージョンの場合はQuicklispのdistをアップデートしてください。

//emlist{
(asdf:component-version (asdf:find-system :rove))
;=> "0.9.1"
//}


この章では読み進めながらREPLで試せるように説明と同時にコード実行例を多く挟んでいます。途中のコード例では特に言及がない限り @<code>{cl-user}パッケージで @<code>{rove} パッケージをインポートした状態を前提としています。

同じ環境で実行するためには、REPLを起動して以下の二行を実行すれば準備完了です。


//emlist{
(ql:quickload :rove)
(use-package :rove)
//}

それではRoveの基本機能から見ていきましょう。

== Roveの基本機能

=== 基本的なアサーション


まずはRoveを使って単純な値の比較をしてみます。

Roveの基本的なマクロ @<code>{ok} はフォームを受け取り、その返り値が真のときは成功、偽のときは失敗となるマクロです。

//emlist{
;; 成功
(ok (eql 1 1))
;-> ✓ Expect (EQL 1 1) to be true.
;=> T

;; 失敗
(ok (eql 1 #\a))
;-> × 0) Expect (EQL 1 #\a) to be true.
;=> NIL
//}

一方で @<code>{ng} は、フォームの内容が偽のときは成功、真のときは失敗となります。

//emlist{
;; 成功
(ng (eql 1 1))
;-> × 1) Expect (EQL 1 1) to be false.
;=> T

;; 失敗
(ng (eql 1 #\a))
;-> ✓ Expect (EQL 1 #\a) to be false.
;=> NIL
//}

ログに出力される @<tt>{Expect ...} の部分を変えることもできます。 @<code>{ok} の第二引数に文字列として表示したい文字列を渡します。

//emlist{
(ok (= 1 1) "1 should be 1")
;-> ✓ 1 should be 1
;=> T
//}

Roveを使ったテストではこの @<code>{ok} や @<code>{ng} を基本単位としてテストプログラムを構成します。この最小単位を「@<b>{アサーション}」と呼びます。

=== テストの定義

アサーションを並べるだけでもプログラムのテストはできますが、量が増えてくるとプログラムのコンポーネント単位や、機能単位での分割をしたほうが見通しがよくなります。また、失敗したときのログもわかりやすくなります。

複数のアサーションをひとまとめにした単位をRoveでは「@<b>{テスト}」と呼びます。テストは @<code>{deftest} を使って定義します。

たとえば関数 @<code>{integerp} のテストをしたいとすると以下のようなテストが考えられます。引数にさまざまな値を与えてその返り値を @<code>{ok} と @<code>{ng} で検証します。例として一つだけ失敗するアサーションを混ぜています。


//emlist{
(deftest integerp
  (ok (integerp 1))
  (ng (integerp 1.0))
  (ok (integerp "1")))
;=> INTEGERP
//}

定義されたテストの実行は @<code>{rove:run-test} を使います。

//emlist{
(run-test 'integerp)
;-> integerp
;     ✓ Expect (INTEGERP 1) to be true.
;     ✓ Expect (INTEGERP 1.0) to be false.
;     × 0) Expect (INTEGERP "1") to be true.
;
;   × 1 of 1 test failed
;
;   0) integerp
;      Expect (INTEGERP "1") to be true.
;        (INTEGERP "1")
;=> NIL
//}

テスト内で失敗したアサーションがあると最後に失敗したアサーションの詳細情報を表示します。エラーが発生した場合はスタックトレースを表示します。

テスト内で複数のテストをまとめるには @<code>{testing} が使えます。アサーションを機能別に適切な説明文をつけてまとめることで関心が分離でき、メンテナンス性の高いテストコードが書けます。また、 @<code>{testing} は自由にネストすることができます。

//emlist{
(deftest integerp
  (testing "should return T for integers"
    (ok (integerp 1))
    (ok (integerp 0))
    (ok (integerp -1))
    (ok (integerp most-positive-fixnum)))

  (testing "should return NIL for floats"
    (ng (integerp 1.0))
    (ng (integerp -0.01))
    (ng (integerp pi))))

(run-test 'integerp)
;-> integerp
;     should return T for integers
;       ✓ Expect (INTEGERP 1) to be true.
;       ✓ Expect (INTEGERP 0) to be true.
;       ✓ Expect (INTEGERP -1) to be true.
;       ✓ Expect (INTEGERP MOST-POSITIVE-FIXNUM) to be true.
;     should return NIL for floats
;       ✓ Expect (INTEGERP 1.0) to be false.
;       ✓ Expect (INTEGERP -0.01) to be false.
;       ✓ Expect (INTEGERP PI) to be false.
;
;   ✓ 1 test completed
;=> NIL
//}

=== テストスイート

一般的な規模のプロジェクトでは、関数は一つではありません。複数のファイル・パッケージに分割された関数があり、それぞれの関数に正常系・異常系の単体テストを書く必要があります。テストも複数の @<code>{deftest} に分割して定義する必要があります。

それらの複数のテストをまとめたものを「@<b>{テストスイート}」と呼びます。Roveではパッケージごとに暗黙的にテストスイートが作られるため、テストをそれぞれのパッケージに分割して記述するだけでテストをひとまとめに扱えます。

//list[my-project/tests/file1][tests/file1.lisp][common-lisp]{
(defpackage #:my-project/tests/file1
  (:use #:cl
        #:rove
        #:my-project/file1))
(in-package #:my-project/tests/file1)

(deftest func1
  (ok (equal (func1 "A") "a")))
//}

//list[my-project/tests/file2][tests/file2.lisp][common-lisp]{
(defpackage #:my-project/tests/file2
  (:use #:cl
        #:rove
        #:my-project/file2))
(in-package #:my-project/tests/file2)

(deftest func2
  (ok (equal (func2 1) "1")))
//}

テストスイート内のすべてのテストを実行するには @<code>{rove:run-suite} を使います。


//emlist{
(run-suite :my-project/tests/file1)
;-> func1
;     ✓ Expect (EQUAL (FUNC1 "A") "a") to be true.
;
;   ✓ 1 test completed
//}

加えてテストスイートにはテスト実行前と終了前をフックする機能があります。

たとえば、テスト実行前に特定のディレクトリが作られていることを保証したい場合は @<code>{setup} を使って以下のように定義できます。

//emlist{
(setup
  (ensure-direcctories-exist #P"/tmp/my-project/"))
//}

さらにテスト終了時にディレクトリを削除したいときには @<code>{teardown} が使えます。

//emlist{
(teardown
  (uiop:delete-directory-tree #P"/tmp/my-project/" :validate t))
//}

これらの定義はパッケージ内であれば先頭でも末尾でも構いません。通常は最初の @<code>{deftest} の前に記述することが多いです。

@<code>{setup} と @<code>{teardown} はテストスイートの実行前と後のそれぞれ一回ずつしか実行されませんが、それぞれのテストの前後で実行させる機能もあります。これは @<code>{defhook} で定義します。

//emlist{
;; 毎テスト実行前に走らせるコード
(defhook :before
  (format t "~&Going to run a test...~%"))

;; 毎テスト実行後に走らせるコード
(defhook :after
  (format t "~&Done.~%"))
//}

== テストに便利なユーティリティ群

Roveではよくテストで使われる手法を簡便にするためのマクロがいくつか用意されています。

=== エラーの発生をテストする

異常系のテストではエラーが発生することをテストしたいときには @<code>{signals} が使えます。 @<code>{signals} は与えられたフォーム内でエラーが発生すると真を返すマクロです。

//emlist{
(defun add (x y) (+ x y))

;; 第二引数に捕捉するコンディションクラスを指定する
(ok (signals (add 1 "2") 'type-error))
;-> ✓ Expect (ADD 1 "2") to signal TYPE-ERROR.

;; 第二引数が指定されないときは ERROR をすべて捕捉する
(ok (signals (add 1 "2")))
;-> ✓ Expect (ADD 1 "2") to signal ERROR.
//}

=== ストリームへの出力をテストする

ストリームへの出力をテストするには @<code>{outputs} を使います。


//emlist{
(defun say-hello (&optional (name "Guest")) (format t "Hi, ~A" name))

(ok (outputs (say-hello "Eitaro") "Hi, Eitaro"))
;-> ✓ Expect (SAY-HELLO "Eitaro") to output "Hi, Eitaro".

(ok (outputs (say-hello) "Hi, Guest"))
;-> ✓ Expect (SAY-HELLO) to output "Hi, Guest".
//}

=== マクロのテストをする

マクロのテストをするには @<code>{expands} を使います。第一引数に展開前のフォームを、第二引数に展開後のフォームを渡します。

単純に @<code>{macroexpand-1} して @<code>{equalp} で比較することもできますが、マクロのテストで厄介なのは、展開されるフォームに gensym が含まれるときです。gensymが含まれると単純な @<code>{equal} で比較することができません。一方で @<code>{expands} はパッケージにインターンされていないシンボル、 @<code>{#:} で始まるシンボルをgensymとして緩いマッチングを行います。


//emlist{
(defmacro defun-addn (n)
  (let ((m (gensym "m")))
    `(defun ,(intern (format nil "ADD~A" n)) (,m)
       (+ ,m ,n))))

(ok (expands '(defun-addn 10)
             `(defun add10 (#:m)
                (+ #:m 10))))
;-> ✓ Expect '(DEFUN-ADDN 10) to be expanded to `(DEFUN ADD10 (#:M) (+ #:M 10)).
//}

== 実践: yubinのテストを追加する

=== テストを書く

tests/main.lisp に関数 @<code>{get-place} のテストを追加します。この関数が正常に動作するケースと、動作しないケースの両方を含めます。たとえば以下のようなケースをテストしてみましょう。

 * 郵便番号を数値で渡して、正しい住所が返ってくるか
 * 郵便番号をハイフン入りの文字列で渡して、正しい住所が返ってくるか
 * 存在しない郵便番号を渡して、エラーとなるか
 * 郵便番号ではない文字列を渡して、エラーとなるか

これをテストコードとしたものが以下です。

//list[yubin/tests/main][tests/main.lisp][common-lisp]{
(defpackage #:yubin/tests/main
  (:use #:cl
        #:yubin
        #:rove))
(in-package #:yubin/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :yubin)' in your Lisp.

(deftest get-place
  (testing "should return the address for a given postal code in a number"
    (ok (equal (yubin:get-place 6380321) "奈良県吉野郡天川村坪内"))
    (ok (equal (yubin:get-place 1500000) "東京都渋谷区")))
  (testing "should return the address for a given postal code in a string"
    (ok (equal (yubin:get-place "6380321") "奈良県吉野郡天川村坪内"))
    (ok (equal (yubin:get-place "150-0000") "東京都渋谷区")))
  (testing "should raise an error for an unknown postal code"
    (ok (signals (yubin:get-place 6068501))))
  (testing "should raise an error for non postal code"
    (ok (signals (yubin:get-place "clfreaks")))))
//}

=== テストの実行


テストシステムをREPLで走らせるには @<code>{asdf:test-system} を使います。


//emlist{
(asdf:test-system :yubin)
;; テストシステムに対して rove:run を使っても同じ
(rove:run :yubin/tests)
//}


コマンドラインから実行するには「rove」コマンドを使います。roveコマンドは第一章の終わりでインストール方法を紹介しています。roveコマンドの引数にASDファイルを渡すとそのシステムの @<code>{asdf:test-system} を呼び出し、テストが実行されます。


//cmd{
$ @<b>{rove yubin.asd}
;; testing 'yubin/tests/main'
  get-place
    should return the address for a given postal code in a number
      ✓ Expect (EQUAL (YUBIN:GET-PLACE 6380321) "奈良県吉野郡天川村坪内") to be true. (685ms)
      ✓ Expect (EQUAL (YUBIN:GET-PLACE 1500000) "東京都渋谷区") to be true. (406ms)
    should return the address for a given postal code in a string
      ✓ Expect (EQUAL (YUBIN:GET-PLACE "6380321") "奈良県吉野郡天川村坪内") to be true. (406ms)
      ✓ Expect (EQUAL (YUBIN:GET-PLACE "150-0000") "東京都渋谷区") to be true. (405ms)
    should raise an error for an unknown postal code
      ✓ Expect (YUBIN:GET-PLACE 6068501) to signal ERROR. (408ms)
    should raise an error for non postal code
      ✓ Expect (YUBIN:GET-PLACE "clfreaks") to signal ERROR. (413ms)

✓ 1 test completed

Summary:
  All 1 test passed.
//}

=== レポートスタイルの変更

Roveのもう一つの特徴は、テストの定義と出力スタイルが分離されていることです。これによりテストの結果出力の形式をテストコードを変更することなく変えることができます。これをRoveでは「レポートスタイル」と呼びます。

レポートスタイルを変更するには @<code>{rove:run} に @<code>{:style} でスタイル名を指定します。

//emlist{
;; デフォルトのレポートスタイル
(rove:run :yubin/tests :style :spec)

;; よりミニマルな表示。アサーションの結果を一つのドットで表現する
(rove:run :yubin/tests :style :dot)

;; 何も出力せず返り値のみ
(rove:run :yubin/tests :style :none)
//}

デフォルトのレポートスタイルは @<code>{:spec} です。これを変更するには @<code>{rove:*default-reporter*} に好きなスタイルを設定します。Lispの初期化ファイルに記述する場合は @<code>{cl-user:*rove-default-reporter*} に設定します。

//emlist{
;; .roswell/init.lisp
(defvar *rove-default-reporter* :dot)
//}

== まとめ

この章ではRoveを紹介し、Common Lispのテストを行う流れを説明しました。Roveではアサーションを最小単位として、テスト、テストスイート、テストシステムに分割してテストを定義します。

最後にRoveによってテストされているOSSライブラリを紹介します。ぜひ参考にしてみてください。

 * Safety-Params@<br>{}https://github.com/fukamachi/safety-params
 * jsonrpc@<br>{}https://github.com/fukamachi/jsonrpc
