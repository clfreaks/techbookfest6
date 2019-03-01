## はじめに
lemでの拡張機能の書き方についてです。
ここではredditの投稿一覧のビューアを作りながらlemでの拡張を書いていきます。
プロジェクト名はposts-listとします。

完成したものが次のようなものになります。

[](./screenshot.png)

上のスクリーンショットでは/r/lispの投稿のリストを取得して表示しています。
選択してリンク先をブラウザで開く機能などをつけていきます。

これからこの拡張をボトムアップ形式で作っていきます。

## 
lemの拡張機能はasdfのシステムとして扱います。
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

lem上で`M-x start-lisp-repl`とするとreplが表示されます。
最初に作成したプロジェクトを読み込んでみます。

```
CL-USER> (ql:quickload :lem-posts-list)
```

エラーが出て読み込めない場合はパスが通っていない可能性が高いです。
デフォルトでは$HOME/common-lispにパスが通ってあるので
posts-listディレクトリを$HOME/common-lisp/以下に配置するのが簡単です。

##
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

##
投稿一覧が取得できるようになったので次はその内容をlemのバッファに表示し選択できるようにします。

投稿一覧用のバッファを作り、そこにfetch-posts関数から返ってきたpostのリストを書き出していきます。
