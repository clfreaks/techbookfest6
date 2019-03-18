# Rove

## セットアップ

まずはREPLでRoveをロードします。RoveはQuicklispに登録されているため、他のライブラリ同様にql:quickloadでインストールとロードができます。

```
(ql:quickload :rove)
```

この本ではRove 0.9.0を使います。各環境のRoveのバージョン番号は以下のように確認できます。古いバージョンの場合はQuicklispのdistをアップデートしてください。

```
(asdf:component-version (asdf:find-system :rove))
;=> "0.9.0"
```

0.9.0より新しいバージョンでは挙動にやや変化があるかもしれませんがご了承ください。

以下の説明では特に言及がない限りcl-userパッケージで `rove` パッケージをインポートした状態を前提としています。

```
(use-package :rove)
```

## 基本的なアサーション

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
✓ 1 should be 1
T
```

Roveを使ったテストではこの `ok` や `ng` を基本単位としてテストプログラムを構成します。この最小単位を「アサーション」と呼びます。

TODO: 他のアサーション signals, outputs, expands の紹介

## テストの定義

TODO: deftest の紹介

