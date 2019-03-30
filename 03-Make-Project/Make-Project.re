
= プロジェクトの作成

Common Lispでライブラリやアプリケーションを開発して他者に配布するには、規定の方法でシステムを構築する必要があります。プロジェクトに必要なファイルを1つずつ書くのは手間がかかりますが、Common Lisp製ライブラリのcl-projectを用いると、プロジェクトに必要なファイル群を生成することができます。

本章では、cl-projectで生成された雛形を編集しながら、小さなCommon Lispプロジェクトを開発する方法を紹介します。

== cl-project

まず、Roswellでcl-projectをインストールします。インストール後は、プロジェクトの雛形を生成するmake-projectコマンドが使えるようになります。

//cmd{
$ ros install fukamachi/cl-project
//}

== プロジェクトの作成例 - 地名検索システムyubin

zipcloudのWeb APIを用いて、郵便番号から地名を検索するシステムを作ります。完成後は、次のように利用できます。

//cmd{
$ ros install clfreaks/yubin
$ yubin 6380321
奈良県吉野郡天川村坪内
//}

//embed[latex]{
\clearpage
//}

=== プロジェクトの雛形生成

まず、プロジェクトの雛形を生成します。プロジェクト名をyubin、依存ライブラリにdexador、jonathan、quri、roveを指定してプロジェクトの雛形を生成します。Roswellから読み込めるように、@<tt>{~/.roswell/local-projects}に移動後、make-projectを実行します。

//cmd{
$ cd ~/.roswell/local-projects
$ make-project yubin --depends-on dexador jonathan quri rove
//}

上記のようにmake-projectコマンドを実行後、システム定義ファイル(yubin.asd)、メインファイル(src/main.lisp)、テストファイル(tests/main.lisp)、READMEファイルが生成されます。

//embed[latex]{
\vspace{1\Cvs}
//}

では、生成されたファイルを編集しながら、簡単なアプリを作成していきましょう。

=== システム定義ファイル(yubin.asd)

Common Lispでは、主にASDF(Another System Definition Facility)を用いてプロジェクトを管理します。システム定義ファイルを規定の方法で記述することにより、プロジェクトの読み込みからテストまで行うことができます。では、make-projectコマンドで生成されたシステム定義ファイル(yubin.asd)を見てみましょう。

//emlist{
(defsystem "yubin"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("dexador"
               "jonathan"
               "rove")
  :components ((:module "src"
                :components
                ((:file "main"))))
  ;; 以下は省略
)
//}

:depends-on と :components に注目してください。:depends-on には、make-projectで指定した依存ライブラリが挿入されます。ここに記入されたライブラリが、Quicklispのアーカイブからダウンロードされて読み込まれます。 :components 内には、:depends-on で記されたライブラリのロード後に読み込むファイルを指定します。ここでは :file "main" と指定されていますが、これはsrcフォルダ内のmain.lispを指します。

//embed[latex]{
\vspace{1\Cvs}
//}

//embed[latex]{
\clearpage
//}

=== メインファイル(main.lisp)

では、@<tt>{main.lisp}を次のように編集しましょう。
  
//emlist{
(defpackage #:yubin ; ①
  (:use #:cl)
  (:import-from #:quri
                #:make-uri)
  (:import-from #:jonathan
                #:parse)
  (:import-from #:dexador)
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

①では、yubinパッケージを定義しています。依存するパッケージを (:import-from #:ライブラリ名) の形式で指定します。依存ライブラリから特定のシンボルを取り込むときは、(:import-from #:ライブラリ名 #:シンボル名) とします。
例えば (:import-from #:jonathan #:parse) という箇所は、jonathanのparse関数を取り込むという意味です。また、後で定義するget-place関数が外部から利用できるように、get-placeをexportします。

②では、get-place関数を定義しています。get-place関数は、引数 @<tt>{zipcode} からURLを作り、HTTPリクエストをして結果のJSONをパースし、結果を住所として文字列で返します。もし結果が返ってこなかった場合にはエラーを投げます。

=== Roswell Script

インストール後にyubinコマンドが使えるように、プロジェクト内のroswellフォルダ内にRoswell Scriptを作成します。Roswell Scriptは、@<tt>{ros init}コマンドで生成される雛形を元に作成します。

//cmd{
$ mkdir roswell && cd roswell
$ ros init yubin.ros
//}

生成された雛形を次のように編集します。

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

③の行は、デフォルトでコメントアウトされていますが、コメントアウトを解除して @<tt>{ql:quickload} に @<tt>{:yubin} を指定します。④では、@<tt>{main} 関数を定義しています。yubinコマンドが呼ばれるとき、この @<tt>{main} 関数が実行されます。

== プロジェクトの共有

プロジェクトの完成後は、プロジェクトをGitHubのリポジトリにpushすることで、プロジェクトを他者と共有することができます。

//cmd{
$ git push -u origin master
$ ros install clfreaks/yubin
$ yubin 6390321
奈良県吉野郡天川村坪内
//}

//embed[latex]{
\clearpage
//}

== package-inferred-system

@<tt>{package-inferred-system}は、ASDFのオプションとして提供されているパッケージ管理方法です。この手法は、全てのファイルはdefpackageで始まり固有のパッケージ名をもつことから、one package per file (1つのファイルにつき1パッケージ)と呼ばれます。パッケージ名をファイルのパス名と合致するように作成して、ファイル内でパッケージの依存関係を記述することで、パッケージ間の依存関係が推測(inferred)されて解決されます。

package-inferred-systemを用いた実例としては、第八章をご参照ください。

== まとめ

本章では、cl-projectで生成されたプロジェクトを元にプロジェクトを作成して、Roswellで作成したプロジェクトを共有する方法を紹介しました。Roswellとcl-projectを合わせて使うと、小さなプロジェクトであれば、プロジェクトの作成から公開がこんなに早くできるのかと思っていただけたら幸いです。
