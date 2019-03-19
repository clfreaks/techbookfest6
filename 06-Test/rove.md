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

TODO: 他のアサーション signals, outputs, expands の紹介

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
