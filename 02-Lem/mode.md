## はじめに
lemでの拡張機能の書き方についてです。
redditの投稿一覧のビューアを作りながらlemでの拡張を見ていきます。
プロジェクト名はposts-listとします。

完成したものが次のようなものになります。

[](./screenshot.png)

上のスクリーンショットでは/r/lispの投稿のリストを取得して表示しています。
選択するとリンク先をブラウザで開く機能などがついています。

## プロジェクトの作成
lemの拡張機能は他のプロジェクトと同じようにasdfのシステムとして扱います。
ベースとするディレクトリの下に拡張機能と同じ名前のディレクトリを用意し、
その下に各ファイルを配置します。

テンプレートを以下のコマンドで生成できます。

$ ros init lem posts-list

これでプロジェクトのベースが出来ました。
ディレクトリツリーは以下のようになります。

```
$ tree posts-list
posts-list
├── lem-posts-list.asd
└── main.lisp
```

`M-x start-lisp-repl`とするとreplが表示されます。
作成したプロジェクトを読み込んでみます。

```
CL-USER> (ql:quickload :lem-posts-list)
```

エラーが出て読み込めない場合はパスが通っていない可能性が高いです。
デフォルトでは$HOME/common-lispにパスが通ってあるので
posts-listディレクトリを$HOME/common-lisp/以下に配置してみてください。

## 記事のリストを取得
subredditを指定してredditの記事をjsonで取得してみます。
一つの記事をpostという構造体にし、postのリストを返す処理を用意します。
これ自体はlemとは関係ないのでposts.lispに分離します。

```
(defpackage #:lem-posts-list/posts
  (:import-from #:jonathan)
  (:import-from #:dexador)
  (:import-from #:trivia
                #:match
                #:plist)
  (:import-from #:quri)
  (:use #:cl)
  (:export #:post-title
           #:post-url
           #:post-author
           #:fetch-posts))
(in-package #:lem-posts-list/posts)

(defstruct post title url author)

(defun extract-posts (data)
  (match data
    ((plist :|data| (plist :|children| children))
     (loop :for child :in children
           :collect (match child
                      ((plist :|data|
                              (plist :|title| title
                                     :|url| url
                                     :|author| author))
                       (make-post :title title
                                  :url url
                                  :author author)))))))

(defun make-posts-url (subreddit)
  (quri:make-uri :scheme "https"
                 :host "reddit.com"
                 :path (format nil "/r/~A/.json" subreddit)))

(defun fetch-posts (subreddit)
  (extract-posts (jojo:parse (dex:get (make-posts-url subreddit)))))
```

lem-posts-list/postsというパッケージに分離しました。
fetch-postsという関数とpostのアクセサを外部から使いたいのでexportします。

fetch-postsはsubreddit名を引数で受け取りpostのリストを返します。
replから動作を確認してみます。

```
CL-USER> (ql:quickload :lem-posts-list/posts)
CL-USER> (lem-posts-list/posts:fetch-posts "lisp")

;; 出力は長いので省略
```

## lemへの表示
投稿一覧が取得できるようになったので次はその内容をlemに表示し選択できるようにします。

投稿一覧用のバッファを作り、そこにfetch-posts関数から返ってきたpostのリストを書き出していきます。
その前にlemで扱うオブジェクトについていくつか見ていきます。

### buffer
バッファはテキストとその色やカーソルの位置、モードなどが入ったオブジェクトです。
通常はファイルを開くときにバッファはそのファイルと関連付けられますが、
ファイルと関連付けずにバッファ自体を作成することも可能です。

バッファは`make-buffer`関数を使うことで作成できます。
ファイルと関連付けられたバッファは`find-file-buffer`関数を使うことで作成できます。

```
(lem:make-buffer "test") ; => #<BUFFER test NIL>
(lem:find-file-buffer "/tmp/hoge") ; => #<BUFFER hoge /tmp/hoge>
```

### point
ポイントはバッファ内の位置を指すオブジェクトです。
主にカーソルなどに使われています。
バッファ内への文字列の挿入や削除に使います。

ポイントを扱う場合はバッファ内に既にあるポイントをコピーして使う事が多いです。

バッファからポイントを得るアクセサは次のようなものがあります。

```
;; バッファの現在の位置のポイントを得る
(buffer-point buffer)

;; バッファの先頭の位置のポイントを得る
(buffer-start-point buffer)

;; バッファの末尾の位置のポイントを得る
(buffer-end-point buffer)
```

ポイントのコピーには`copy-point`関数を使います。
`(copy-point point &optional kind)`

ポイントはスティッキーな動作をします。
そのポイントより前の位置に文字列を挿入するとその分右へずれていき、削除すると左にずれていきます。
`kind`にはバッファ編集時のオフセットを計算するときに使います。
`kind`が:left-insertingならポイントと同じ位置に文字列を挿入したときに右にずれ、:right-insertingならそのままです。
`kind`が:temporaryの場合は何も行いません。
`kind`を指定しばければ渡された`point`と同じ値になります。

`kind`が:temporary以外ならpointをbufferが保持しておく必要があるので不要になったら明示的に削除しなければいけません。
削除には`delete-point`関数を使います。

```
(let ((point (copy-point (buffer-point buffer) kind)))
  (unwind-protect ...
    (delete-point point)))
```

このために`with-point`マクロを用意しています。
```
(with-point ((point (buffer-point buffer) kind))
  ...)
```

`with-point`の`kind`を省略した場合は:temporaryになります。
