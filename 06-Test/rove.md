# Rove

## セットアップ

まずはREPLでRoveをロードします。RoveはQuicklispに登録されているため、他のライブラリ同様にql:quickloadでインストールとロードができます。

```
(ql:quickload :rove)
```

この本ではRove 0.9.1を使います。各環境のRoveのバージョン番号は以下のように確認できます。古いバージョンの場合はQuicklispのdistをアップデートしてください。

```
(asdf:component-version (asdf:find-system :rove))
;=> "0.9.1"
```

0.9.1より新しいバージョンでは挙動にやや変化があるかもしれませんがご了承ください。

以下の説明では特に言及がない限りcl-userパッケージで `rove` パッケージをインポートした状態を前提としています。

```
(use-package :rove)
```

## Roveの基本機能

### 基本的なアサーション

まずは単純な値の比較から行いましょう。

`ok` はフォームを受け取り、その返り値が真のときは成功、偽のときは失敗となるマクロです。

```
;; 成功
(ok (eql 1 1))
;-> ✓ Expect (EQL 1 1) to be true.
;=> T

;; 失敗
(ok (eql 1 #\a))
;-> × 0) Expect (EQL 1 #\a) to be true.
;=> NIL
```

一方で `ng` は、フォームの内容が偽のときは成功、真のときは失敗となります。

```
;; 成功
(ng (eql 1 1))
;-> × 1) Expect (EQL 1 1) to be false.
;=> T

;; 失敗
(ng (eql 1 #\a))
;-> ✓ Expect (EQL 1 #\a) to be false.
;=> NIL
```

`Expect ...` の部分を変えることもできます。 `ok` の第二引数に文字列として表示したい文字列を渡します。

```
(ok (= 1 1) "1 should be 1")
;-> ✓ 1 should be 1
;=> T
```

Roveを使ったテストではこの `ok` や `ng` を基本単位としてテストプログラムを構成します。この最小単位を「アサーション」と呼びます。

異常系のテストではエラーが発生することをテストしたいこともあるでしょう。 `signals` は与えられたフォーム内でエラーが発生すると真を返すマクロです。

```
(defun add (x y) (+ x y))

;; 第二引数に捕捉するコンディションクラスを指定する
(ok (signals (add 1 "2") 'type-error))
;-> ✓ Expect (ADD 1 "2") to signal TYPE-ERROR.

;; 第二引数が指定されないときは ERROR をすべて捕捉する
(ok (signals (add 1 "2")))
;-> ✓ Expect (ADD 1 "2") to signal ERROR.
```

出力をテストするには `outputs` を使います。

```
(defun say-hello (&optional (name "Guest")) (format t "Hi, ~A" name))

(ok (outputs (say-hello "Eitaro") "Hi, Eitaro"))
;-> ✓ Expect (SAY-HELLO "Eitaro") to output "Hi, Eitaro".

(ok (outputs (say-hello) "Hi, Guest"))
;-> ✓ Expect (SAY-HELLO) to output "Hi, Guest".
```

マクロのテストをするには `expands` を使います。マクロのテストで厄介なのは、展開されるフォームに gensym が含まれることです。gensymが含まれると単純な `equal` で比較することができません。一方で `expands` はパッケージにインターンされていないシンボル、 `#:` で始まるシンボルをgensymとして緩いマッチングを行います。

```
(defmacro defun-addn (n)
  (let ((m (gensym "m")))
    `(defun ,(intern (format nil "ADD~A" n)) (,m)
       (+ ,m ,n))))

(ok (expands '(defun-addn 10)
             `(defun add10 (#:m)
                (+ #:m 10))))
;-> ✓ Expect '(DEFUN-ADDN 10) to be expanded to `(DEFUN ADD10 (#:M) (+ #:M 10)).
```

### テストの定義

アサーションを並べるだけでもプログラムのテストはできますが、量が増えてくるとプログラムのコンポーネント単位や、機能単位での分割をしたほうが見通しがよくなります。また、失敗したときのログもわかりやすくなります。

複数のアサーションをひとまとめにした単位をRoveでは「テスト」と呼びます。テストは `deftest` を使って定義します。

たとえば関数 `integerp` のテストをしたいとすると以下のようなテストが考えられます。引数にさまざまな値を与えてその返り値を `ok` と `ng` で検証します。例として一つだけ失敗するアサーションを混ぜています。

```
(deftest integerp
  (ok (integerp 1))
  (ng (integerp 1.0))
  (ok (integerp "1")))
;=> INTEGERP
```

定義されたテストの実行は `rove:run-test` を使います。

```
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
```

テスト内で失敗したアサーションがあると最後に失敗したアサーションの詳細情報を表示します。エラーが発生した場合はスタックトレースを表示します。

テスト内で複数のテストをまとめるには `testing` が使えます。アサーションを機能別に適切な説明文をつけてまとめることで関心が分離でき、メンテナンス性の高いテストコードが書けます。また、 `testing` は自由にネストすることができます。

```
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
```

### テストスイート

一般的な規模のプロジェクトでは、関数は一つではありません。複数のファイル・パッケージに分割された関数があり、それぞれの関数に正常系・異常系の単体テストを書く必要があります。テストも複数の `deftest` に分割して定義する必要があります。

それらの複数のテストをまとめたものを「テストスイート」と呼びます。

Roveではパッケージごとに暗黙的にテストスイートが作られるため、テストをそれぞれのパッケージに分割して記述するだけでテストをひとまとめに扱えます。

```
;; tests/file1.lisp
(defpackage #:my-project/tests/file1
  (:use #:cl
        #:rove
        #:my-project/file1))
(in-package #:my-project/tests/file1)

(deftest func1
  (ok (equal (func1 "A") "a")))

;; tests/file2.lisp
(defpackage #:my-project/tests/file2
  (:use #:cl
        #:rove
        #:my-project/file2))
(in-package #:my-project/tests/file2)

(deftest func2
  (ok (equal (func2 1) "1")))
```

テストスイート内のすべてのテストを実行するには `rove:run-suite` を使います。

```
(run-suite :my-project/tests/file1)
;-> func1
;     ✓ Expect (EQUAL (FUNC1 "A") "a") to be true.
;
;   ✓ 1 test completed
```

加えてテストスイートにはテスト実行前と終了前をフックする機能があります。

たとえば、テスト実行前に特定のディレクトリが作られていることを保証したい場合は `setup` を使って以下のように定義できます。

```
(setup
  (ensure-direcctories-exist #P"/tmp/my-project/"))
```

さらにテスト終了時にディレクトリを削除したいときには `teardown` が使えます。

```
(teardown
  (uiop:delete-directory-tree #P"/tmp/my-project/" :validate t))
```

これらの定義はパッケージ内であれば先頭でも末尾でも構いません。通常は最初の `deftest` の前に記述することが多いです。

`setup` と `teardown` はテストスイートの実行前と後のそれぞれ一回ずつしか実行されませんが、それぞれのテストの前後で実行させる機能もあります。これは `defhook` で定義します。

```
;; 毎テスト実行前に走らせるコード
(defhook :before
  (format t "~&Going to run a test...~%"))

;; 毎テスト実行後に走らせるコード
(defhook :after
  (format t "~&Done.~%"))
```

## テストシステムの作成

### ASDFの設定

ASDFシステムにRoveを組み込むにはASDファイルにテストシステムを定義します。

以下では src に本体コード、 tests にテストコードがあるときのASDFシステム定義例です。本体のシステム名は `"my-project"`、テストシステム名は `"my-project/tests"` です。

```
;; my-project.asd
(defsystem "my-project"
  :class :package-inferred-system
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :description "My sample project for Rove"
  :license "BSD 2-Clause"
  :pathname "src"
  :components ((:file "file1")
               (:file "file2"))
  :in-order-to ((test-op (test-op "my-project/tests"))))

(defsystem "my-project/tests"
  :depends-on ("my-project"
               "rove")
  :pathname "tests"
  :components ((:file "file1")
               (:file "file2"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
```

本体システムに追加している `:in-order-to` は `asdf:test-system` したときにテストシステムを走らせるための設定です。テストシステムにある `:perform` はそのときにRoveのテストを走らせるための記述です。

### テストの実行

テストシステムをREPLで走らせるには `asdf:test-system` を使います。

```
(asdf:test-system :my-project)
```

コマンドラインから実行するには「rove」コマンドを使います。roveコマンドはRoswellでRoveをインストールすると ~/.roswell/bin/rove にコピーされます。

```
$ ros install rove
```

roveコマンドの引数にASDファイルを渡すとそのシステムの `asdf:test-system` を呼び出し、テストが実行されます。

```
$ rove my-project.asd
```

TODO: レポートスタイルの説明
