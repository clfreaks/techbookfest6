# プロジェクトの作成

Common Lispでライブラリやアプリケーションを開発し、配布するためには、ある程度決まったファイル構成のプロジェクトを作る必要があります。
プロジェクトに必要なファイルを1つずつ用意するのは手間のかかる作業ですが、Common Lisp製ライブラリの **cl-project** を用いることで、一般的なプロジェクトに最低限必要なファイル構成の雛形を生成することができます。

本章では、cl-projectで生成されたプロジェクトの雛形を元に、小さなCommon Lispプロジェクトを開発していく方法を示します。

## cl-project

まず、Roswellを用いてcl-projectをインストールします。

```
$ ros install fukamachi/cl-project

$ make-project
Usage:
    make-project /home/user/common-lisp/sample --name sample --description "sample project." --author "Your name" --license LLGPL --depends-on alexandria split-sequence
```

上記コマンドでcl-projectインストールすると、プロジェクトの雛形を生成するコマンド `make-project` が使えるようになります。

## プロジェクトの作成例 - 地名検索システムyubin

この章ではごく小さなプロジェクトの例として、郵便番号データ配信サービスzipcloudのWeb APIを利用して、郵便番号から地名を検索するシステム `yubin` を作ります。
完成後は、以下のようにRoswellからプロジェクトをインストールし、コマンドとして実行できるようになります。

```
$ ros install clfreaks/yubin
$ yubin 6380321

奈良県吉野郡天川村坪内
```

## プロジェクトの雛形生成

まず、cl-project付属の `make-project` コマンドを用いてプロジェクトの雛形を生成します。
ここでは、プロジェクト名を `yubin` とし、依存ライブラリとしてHTTPクライアントの `Dexador`、JSONライブラリの `Jonathan` を指定してプロジェクトを生成します。

この際、生成したプロジェクトがRoswellのLisp処理系から読み込めるように、`~/.roswell/local-projects` に移動してからmake-projectを実行します。
あるいは、make-projectの結果できたディレクトリへのシンボリックリンクを`~/.roswell/local-projects`に置きます。

```
$ cd ~/.roswell/local-projects
$ make-project yubin --depends-on dexador jonathan
$ tree yubin

yubin
├── README.markdown
├── README.org
├── src
│   └── main.lisp
├── tests
│   └── main.lisp
└── yubin.asd
```

上記のようにmake-projectコマンドを実行すると、システム定義ファイル( `yubin.asd` )、メインファイル( `src/main.lisp` )、テストファイル( `tests/main.lisp` )、READMEファイル( `README.markdown`、 `README.org` )が生成されます。READMEファイルはMarkdown形式とOrg形式の両方が生成されますが、通常はどちらか一方を使用します。

では、生成されたファイルを編集しながら、簡単なアプリケーションを作成していきましょう。

### システム定義ファイル(yubin.asd)

Common Lispでは、**ASDF**(Another System Definition Facility)と呼ばれるソフトウェアを用いてプロジェクトを定義し、依存関係の解決を行います。
ASDFではシステム定義ファイルを記述することにより、プロジェクトの読み込みやテストの実行などを行うことができます。

では、`make-project` コマンドで生成されたシステム定義ファイル `yubin.asd` の内容を見てみましょう。

```common-lisp
(defsystem "yubin"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("dexador"
               "jonathan")
  :components ((:module "src"
                :components
                ((:file "main"))))
  ;; 以下は省略
)
```

ここで、`:depends-on` と `:components` に注目してください。

`:depends-on` には、`make-project` で指定した依存ライブラリが入っています。
`yubin` をロードすると、ここで指定されているライブラリが自動的にQuicklispのアーカイブからダウンロードされ、ロードされます。
`:components` には、依存ライブラリのロード後に読み込むファイルを指定します。
ここでは `(:file "main")` と指定されていますが、これは `src` フォルダ内の `main.lisp` を指しています。

### メインファイル(main.lisp)

では、`main.lisp` を次のように編集しましょう。

```common-lisp
(defpackage #:yubin ; ①
  (:use #:cl)
  (:import-from #:jonathan #:parse)
  (:export #:get-place))
(in-package #:yubin)

(defun get-place (zipcode) ; ②
  (let* ((url (quri:make-uri :defaults "http://zipcloud.ibsnet.co.jp/api/search"
                             :query `(("zipcode" . ,zipcode))))  ; QuriでURLを作る
         (response (parse (dex:get url)))                        ; HTTPリクエストを行う
         (result (first (getf response :|results|))))
    (if result
        (concatenate 'string                                     ; 結果を返す
                     (getf result :|address1|)
                     (getf result :|address2|)
                     (getf result :|address3|))
        (error (format nil "~A: ~S (Code=~A)"                    ; エラーにする
                       (getf response :|message|)
                       zipcode
                       (getf response :|status|))))))
```

①では、`yubin` パッケージを定義しています。
外部パッケージから特定のシンボルをインポートするときには、`(:import-from #:<パッケージ名> #:<シンボル名>)` の形式でシンボル名を指定します。こうすることで、呼び出し時にパッケージ名をシンボルの前に付ける必要がなくなります。
例えば、`yubin` のパッケージ定義の中で `(:import-from #:jonathan #:parse)` と指定しておくことで、`jonathan:parse` ではなく、単に `parse` として呼び出すことができます。

また、`:export` の後に外部へ公開するシンボルを指定します。後で定義する `get-place` 関数が外部から利用できるように、`#:get-place` を指定しておきます。

②では、`get-place` 関数を定義しています。`get-place` 関数は、引数 `zipcode` からURLを作り、zipcloudのWeb APIに対してHTTPリクエストし、レスポンスのJSONをパースし、結果の住所を文字列として返します。もし結果が返ってこなかった場合にはエラーを発生させます。

なお、`quri`はURLを扱うためのライブラリで、`dexador`の依存ライブラリとして自動的にロードされるため、`depends-on`に明示的に指定する必要はありません。

### Roswell Script

第1章で解説したように、プロジェクト直下の `roswell` ディレクトリ内にRoswell Scriptを作っておくことで、このパッケージをRoswellからインストールしたときに、`yubin` コマンドが使えるようになります。Roswell Scriptは、`ros init` コマンドで生成される雛形を元に作成します。

```bash
$ mkdir roswell && cd roswell
$ ros init yubin.ros
```

生成されたファイルを次のように編集します。

```common-lisp
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp (ql:quickload '(:yubin) :silent t))  ; ③

(defpackage :ros.script.yubin.3761982565
  (:use :cl))
(in-package :ros.script.yubin.3761982565)

(defun main (zipcode &rest argv)  ; ④
  (declare (ignorable argv))
  (handler-case
      (format t "~&~A~%" (yubin:get-place zipcode))
    (error (e)
      (format *standard-output* "~&Error: ~A~%" e)
      (uiop:quit -1))))
```

③の行は、デフォルトでコメントアウトされていますが、コメントアウトを解除して `ql:quickload` に `:yubin` を指定します。

④では、`main` 関数を定義しています。`yubin` コマンドが呼ばれるとき、この `main` 関数が実行されます。

## プロジェクトの共有

プロジェクトが完成したら、GitHubのリポジトリに登録しておきます。こうすることで、以降はRoswell経由でインストールできるようになり、プロジェクトを他者と共有することができます。

```bash
$ git push -u origin master
$ ros install clfreaks/yubin
$ yubin 6380321
奈良県吉野郡天川村坪内
```

## package-inferred-system

**package-inferred-system** は、ASDFのオプション機能として提供されているパッケージ管理方法です。package-inferred-systemでは、プロジェクト以下の全ての `.lisp` ファイルでパッケージが定義されます。
パッケージ名をファイルのパスと合致するように作成し、`defpackage` 内の `import-from` に依存するパッケージを記述することで、パッケージ間の依存関係が自動的に推測(inferred)されて解決されます。

package-inferred-systemを用いた実例としては、第8章をご参照ください。

## まとめ

本章では、cl-projectで生成された雛形を元にプロジェクトを作成し、Roswellからインストールできるようになるまでの方法を紹介しました。Roswellとcl-projectを合わせて使うことで、プロジェクトの作成から公開がこんなに早くできるのかと思っていただければ幸いです。
