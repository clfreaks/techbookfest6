# プロジェクトの作成

## cl-project

本章では、Common Lisp製ライブラリcl-projectを用いて、小さなCommon Lispプロジェクトを作成する方法を紹介します。cl-projectは、Common Lispプロジェクトの雛形を生成するライブラリです。プロジェクトのシステム定義ファイル、メインファイル、テストファイル、READMEファイルを生成します。

では、cl-projectをRoswellでインストールしましょう。Roswellでcl-projectをインストールすると、make-projectコマンドが使えるようになります。

```
$ ros install fukamachi/cl-project
```

### プロジェクトの雛形生成

まず、Roswellからプロジェクトを読み込めるように、`~/.roswell/local-projects`に移動します。make-projectコマンドを用いて、プロジェクト名をyubin、依存ライブラリにdexador、jonathan、roveを指定してプロジェクトの雛形を生成します。

```
$ cd ~/.roswell/local-projects
$ make-project yubin --depends-on dexador jonathan rove
```

上記のようにmake-projectコマンドを実行後、システム定義ファイル(yubin.asd)、メインファイル(src/main.lisp)、テストファイル(tests/main.lisp)、READMEファイルが生成されます。

//embed[latex]{
\clearpage
//}

### ASDF

Common Lispでは、主にASDF(Another System Definition Facility)を用いてプロジェクトを管理します。ASDFは、Common Lispのシステム管理ツールであり、主要な処理系にデフォルトで組み込まれています。システム定義ファイルを規定の方法で記述することにより、プロジェクトの読み込みからテストまで行うことができます。

### プロジェクトの作成例 - 地名検索システムyubin

では、生成されたファイルを編集しながら、簡単なアプリを作成します。zipcloudのWeb APIを用いて、郵便番号から地名を検索するシステムを作ります。完成後は、次のように利用できます。

```
$ ros install clfreaks/yubin
$ yubin 6380321
奈良県吉野郡天川村坪内
```

cl-projectで生成されるシステム定義ファイルでは、main.lispを最初に読み込むように設定されています。`main.lisp`を次のように編集します。

```
(defpackage #:yubin  ; ①
  (:use #:cl)
  (:import-from #:jonathan)
  (:import-from #:dexador)
  (:export #:get-place)) 
(in-package #:yubin)

(defun get-place (zipcode)
  (let* ((url (format nil "http://zipcloud.ibsnet.co.jp/api/search?zipcode=~A" zipcode))  ; ②
         (data (reverse (car (fourth (jonathan:parse (dex:get url)))))))  ; ③
    (when data
      (concatenate 'string (first data) (third data) (fifth data)))))  ; ④
```

①では、yubinパッケージを定義しています。JSONのパースとHTTPのGETメソッドのために、jonathanパッケージとdexadorパッケージをimport-fromで指定します。また、後で定義するget-place関数が外部から利用できるように、get-placeをexportします。

yubinパッケージにin-packageした後、郵便番号(zipcode)から地名を返す関数`get-place`を定義します。②では、zipcloudのURLと引数zipcodeの値を連結して、GETメソッドのリクエスト先をurlに設定します。③では、リクエストした結果から必要な情報をdataに設定します。④では、③で設定したdataから文字列を抜き出した値を連結して、値を返します。

//embed[latex]{
\clearpage
//}

インストール後にyubinコマンドが使えるように、プロジェクト内のroswellフォルダ内にRoswell Scriptを作成します。Roswell Scriptは、`ros init`コマンドで生成される雛形を元に作成します。

```
$ mkdir roswell && cd roswell
$ ros init yubin.ros
Successfully generated: yubin.ros
```

生成された雛形を次のように編集します。

```
#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#
(progn ;;init forms
  (ros:ensure-asdf)
  #+quicklisp (ql:quickload '(:yubin) :silent t))   ; ⑤

(defpackage :ros.script.yubin.3761982565
  (:use :cl))
(in-package :ros.script.yubin.3761982565)

(defun main (zipcode &rest argv) ; ⑥
  (declare (ignorable argv))
  (let ((place (yubin:get-place zipcode)))
    (if place
        (format t "~&~A~%" place)
        (format t "~&見つかりませんでした~%"))))
```

⑤では、デフォルトでコメントアウトされていますが、コメントアウトを解除してql:quickloadにyubinを指定します。⑥では、main関数を定義しています。yubinコマンドが呼ばれるとき、このmain関数が実行されます。

### プロジェクトの共有

プロジェクトの完成後は、プロジェクトをGitHubのリポジトリにpushすることで、プロジェクトを他者と共有することができます。

```
$ git push -u origin master
$ ros install clfreaks/yubin
$ yubin 6390321
奈良県吉野郡天川村坪内
```

//embed[latex]{
\clearpage
//}

## package-inferred-system

`package-inferred-system`は、ASDFのオプションとして提供されているパッケージ管理方法です。この手法は、全てのファイルはdefpackageで始まり固有のパッケージ名をもつことから、one package per file (1つのファイルにつき1パッケージ)と呼ばれます。パッケージ名をファイルのパス名と合致するように作成して、ファイル内でパッケージの依存関係を記述することで、パッケージ間の依存関係が推測(inferred)されて解決されます。

この手法を用いたパッケージ管理方の利用法については、四章と八章の実例を参照してください。

## まとめ

本章では、cl-projectで生成されたプロジェクトを元にプロジェクトを作成して、Roswellで作成したプロジェクトを共有する方法を紹介しました。Roswellとcl-projectを合わせて使うと、小さなプロジェクトであれば、プロジェクトの作成から公開がこんなに早くできるのかと思っていただけたら幸いです。
