
= プロジェクトの作成

Common Lispでライブラリやアプリケーションを開発して他者に配布するには、ある決まったファイル構成でプロジェクトを作る必要があります。
プロジェクトに必要なファイルを1つずつ用意するのは手間のかかる作業ですが、Common Lisp製ライブラリの@<b>{cl-project}を用いることで、最低限必要なファイルを一通り生成することができます。

本章では、cl-projectで生成されたプロジェクトの雛形を元に、小さなCommon Lispプロジェクトを開発していきます。

== cl-project

まず、Roswellでcl-projectをインストールします。

//cmd{
$ ros install fukamachi/cl-project
//}

cl-projectインストール後は、プロジェクトの雛形を生成するコマンド@<tt>{make-project}が使えるようになります。


== プロジェクトの作成例 - 地名検索システムyubin

この章では小さなプロジェクトの例として、zipcloudのWeb APIを用いて、郵便番号から地名を検索するシステムを作ります。
完成後は、次のようにインストールし、コマンドとして実行できるようになります。

//cmd{
$ ros install clfreaks/yubin
$ yubin 6380321
奈良県吉野郡天川村坪内
//}

//embed[latex]{
\clearpage
//}

=== プロジェクトの雛形生成

まず、cl-project付属の@<tt>{make-project}コマンドを用いてプロジェクトの雛形を生成します。
ここでは、プロジェクト名を@<tt>{yubin}とし、依存ライブラリとしてDexador、Jonathanを指定してプロジェクトを生成します。
DexadorはHTTPクライアント、JonathanはJSONを扱うためのライブラリです。
この際、生成したプロジェクトをRoswellから読み込めるように、@<tt>{~/.roswell/local-projects}に移動してから@<tt>{make-project}を実行します。

//embed[latex]{
\vspace{-0.5\Cvs}
//}

//cmd{
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
//}

上記のように@<tt>{make-project}コマンドを実行すると、システム定義ファイル(@<tt>{yubin.asd})、メインファイル(@<tt>{src/main.lisp})、テストファイル(@<tt>{tests/main.lisp})、READMEファイルが生成されます。

では、生成されたファイルを編集しながら、簡単なアプリケーションを作成していきましょう。

=== システム定義ファイル(yubin.asd)

Common Lispでは、@<b>{ASDF}(Another System Definition Facility)と呼ばれるソフトウェアを用いてプロジェクトを定義し、依存関係の解決を行います。
ASDFではシステム定義ファイルを記述することにより、プロジェクトの読み込みやテストの実行などを行うことができます。

では、@<tt>{make-project}コマンドで生成されたシステム定義ファイル@<tt>{yubin.asd}の内容を見てみましょう。

//embed[latex]{
\vspace{-0.5\Cvs}
//}

//emlist{
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
//}
ここで、@<tt>{:depends-on}と@<tt>{:components}に注目してください。

@<tt>{:depends-on}には、@<tt>{make-project}で指定した依存ライブラリが入っています。
@<tt>{yubin}をロードすると、ここで指定されているライブラリが自動的にQuicklispのアーカイブからダウンロードされ、ロードされます。
@<tt>{:components}には、依存ライブラリのロード後に読み込むファイルを指定します。
ここでは@<tt>{(:file "main")}と指定されていますが、これは@<tt>{src}フォルダ内の@<tt>{main.lisp}を指しています。

=== メインファイル(main.lisp)

では、@<tt>{main.lisp}を次のように編集しましょう。
  
//emlist{
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

//}

①では、@<tt>{yubin}パッケージを定義しています。
外部パッケージから特定のシンボルをインポートするときには、@<tt>{(:import-from #:<パッケージ名> #:<シンボル名>)}の形式でシンボル名を指定します。こうすることで、呼び出し時にパッケージ名をシンボルの前に付ける必要がなくなります。
例えば、@<tt>{yubin}のパッケージ定義の中で@<tt>{(:import-from #:jonathan #:parse)}と指定しておくことで、@<tt>{jonathan:parse}ではなく、単に@<tt>{parse}として呼び出すことができます。

また、@<tt>{:export}の後に外部へ公開するシンボルを指定します。後で定義する@<tt>{get-place}関数が外部から利用できるように、@<tt>{#:get-place}を指定しておきます。

②では、@<tt>{get-place}関数を定義しています。@<tt>{get-place}関数は、引数 @<tt>{zipcode} からURLを作り、zipcloudのWeb APIに対してHTTPリクエストし、レスポンスのJSONをパースし、結果の住所を文字列として返します。もし結果が返ってこなかった場合にはエラーを発生させます。

//embed[latex]{
\clearpage
//}

=== Roswell Script

1.5節で解説したように、プロジェクト直下の@<tt>{roswell}ディレクトリ内にRoswell Scriptを作っておくことで、このパッケージをRoswellからインストールしたときに、@<tt>{yubin}コマンドが使えるようになります。Roswell Scriptは、@<tt>{ros init}コマンドで生成される雛形を元に作成します。

//cmd{
$ mkdir roswell && cd roswell
$ ros init yubin.ros
//}

生成されたファイルを次のように編集します。

//emlist{
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
//}

③の行は、デフォルトでコメントアウトされていますが、コメントアウトを解除して @<tt>{ql:quickload} に @<tt>{:yubin} を指定します。④では、@<tt>{main} 関数を定義しています。@<tt>{yubin}コマンドが呼ばれるとき、この @<tt>{main} 関数が実行されます。

== プロジェクトの共有

プロジェクトが完成したら、GitHubのリポジトリに登録しておきます。こうすることで、以降はRoswell経由でインストールできるようになり、プロジェクトを他者と共有することができます。

//embed[latex]{
\clearpage
//}

//cmd{
$ git push -u origin master
$ ros install clfreaks/yubin
$ yubin 6390321
奈良県吉野郡天川村坪内
//}

== package-inferred-system

@<b>{package-inferred-system}は、ASDFのオプション機能として提供されているパッケージ管理方法です。package-inferred-systemでは、プロジェクト以下の全ての@<tt>{.lisp}ファイルでパッケージが定義されます。
パッケージ名をファイルのパスと合致するように作成し、@<tt>{defpackage}内の@<tt>{import-from}に依存するパッケージを記述することで、パッケージ間の依存関係が自動的に推測(inferred)されて解決されます。

package-inferred-systemを用いた実例としては、第8章をご参照ください。

== まとめ

本章では、cl-projectで生成された雛形を元にプロジェクトを作成し、Roswellからインストールできるようになるまでの方法を紹介しました。Roswellとcl-projectを合わせて使うことで、プロジェクトの作成から公開がこんなに早くできるのかと思っていただれば幸いです。
