(in-package :cl-user)

(ql:quickload :dexador)

(defparameter *html-source* (dex:get "http://lisp.org/"))

;; "<HTML>
;; <HEAD>
;;   <title>John McCarthy, 1927-2011</title>
;;   <STYLE type=\"text/css\">
;;     BODY {text-align: center}
;;   </STYLE>
;; </HEAD>
;; <BODY>
;; <h1>John McCarthy</h1>
;; <img src=\"jmccolor.jpg\" alt=\"a picture of John McCarthy, from his website\"/>
;; <h3>1927-2011</h3>
;; <br><br>
;; <a href=\"http://www-formal.stanford.edu/jmc/\">John McCarthy's Home Page</a><br>
;; <a href=\"http://news.stanford.edu/news/2011/october/john-mccarthy-obit-102511.html\">Obituary</a>
;; </BODY>
;; </HTML>
;; "
;; 200
;; #<HASH-TABLE :TEST EQUAL :COUNT 10 {100D0F5C83}>
;; #<QURI.URI.HTTP:URI-HTTPS https://lisp.org/>
;; #<CL+SSL::SSL-STREAM for #<FD-STREAM for "socket 192.168.11.253:35804, peer: 144.76.156.38:443" {100C7575B3}>>

;;; Plump

(ql:quickload '(:plump :clss))

(defparameter *root-node* (plump:parse *html-source*))
;; *root-node* => #<PLUMP-DOM:ROOT {1008638843}>

(plump:children *root-node*)
;; => #(#<PLUMP-DOM:ELEMENT HTML {1008AE94D3}> #<PLUMP-DOM:TEXT-NODE {1008AF2EF3}>)

(plump:parent (aref (plump:children *root-node*) 0))
;; => #<PLUMP-DOM:ROOT {1008AE8F93}>

(defun concat-node-text (node)
  (let ((text-list nil))
    (plump:traverse node
                    (lambda (node) (push (plump:text node) text-list))
                    :test #'plump:text-node-p)
    (apply #'concatenate 'string (nreverse text-list))))

(concat-node-text *root-node*)

(plump:traverse *root-node*
                (lambda (node)
                  (setf (plump:text node)
                        (string-upcase (plump:text node))))
                :test #'plump:text-node-p)

(concat-node-text *root-node*)

;;; CLSS
(defparameter *img-nodes* (clss:select "img" *root-node*))
(defparameter *img-node* (aref *img-nodes* 0))
(describe *img-node*)

;; #<PLUMP-DOM:ELEMENT img {100D7DAB63}>
;;   [standard-object]

;; Slots with :INSTANCE allocation:
;;   %PARENT                        = #<PLUMP-DOM:ELEMENT BODY {100D7D8EC3}>
;;   %CHILDREN                      = #()
;;   %TAG-NAME                      = "img"
;;   %ATTRIBUTES                    = #<HASH-TABLE :TEST EQUALP :COUNT 2 {100D7DA2F3}>

(defun print-node-attributes (node)
  (maphash (lambda (key value)
             (format t "key: ~A, value: ~A~%" key value))
           (plump:attributes node)))

(print-node-attributes *img-node*)

;; key: src, value: jmccolor.jpg
;; key: alt, value: a picture of John McCarthy, from his website

(plump:attribute *img-node* "alt")
;; "a picture of John McCarthy, from his website"

(plump:attribute *img-node* "src")
;; "jmccolor.jpg"


(loop for a-node across (clss:select "a" *root-node*)
      collect (plump:attribute a-node "href"))

;; ("http://www-formal.stanford.edu/jmc/"
;;  "http://news.stanford.edu/news/2011/october/john-mccarthy-obit-102511.html")

(map 'list (lambda (a-node)
             (plump:attribute a-node "href"))
     (clss:select "a" *root-node*))

;;; Select by CSS class/id

(defparameter *clojure-root-node*
  (plump:parse (dex:get "https://clojure.org/")))

(clss:select ".w-section" *clojure-root-node*)
;; #(#<PLUMP-DOM:ELEMENT div {1017135813}> #<PLUMP-DOM:ELEMENT div {1017141093}>
;;   #<PLUMP-DOM:ELEMENT div {101714B7E3}> #<PLUMP-DOM:ELEMENT div {101716C8F3}>
;;   #<PLUMP-DOM:ELEMENT div {1017186913}> #<PLUMP-DOM:ELEMENT div {10171A9623}>
;;   #<PLUMP-DOM:ELEMENT div {10171CFF53}> #<PLUMP-DOM:ELEMENT div {10171DFC83}>)

(clss:select "div.clj-intro-message" *clojure-root-node*)
;; #(#<PLUMP-DOM:ELEMENT div {101A9D53F3}>)

(clss:select "body > div.w-nav.clj-navbar > div.w-container > a > img"
             *clojure-root-node*)
;; #(#<PLUMP-DOM:ELEMENT img {10169ACD33}>)

(concat-node-text *clj-intro-message*)

(clss:select "p" *clj-intro-message*)
;; #(#<PLUMP-DOM:ELEMENT p {1017150793}> #<PLUMP-DOM:ELEMENT p {1017152A43}>
;;   #<PLUMP-DOM:ELEMENT p {1017154823}> #<PLUMP-DOM:ELEMENT p {10171558B3}>)

(concat-node-text (aref (clss:select "h2" *clj-intro-message*) 0))
;; "The Clojure Programming Language"

(clss:select "#wf-form-Search-Form" *clojure-root-node*)
;; #(#<PLUMP-DOM:ELEMENT form {1017138953}>)

;;; define-psude-selector

(clss:define-pseudo-selector outside-link (node)
  (let ((href (plump:attribute node "href")))
    (and href (cl-ppcre:scan "^(http|https)://" href))))

;; it is a predicate
(clss:define-pseudo-selector outside-link (node)
  (let ((href (plump:attribute node "href")))
    (and href (if (cl-ppcre:scan "^(http|https)://" href) t nil))))

(clss:select "a:outside-link" *clojure-root-node*)
(clss:select ":outside-link" *clojure-root-node*)

(concat-node-text
 (aref
  (clss:select
  "#file-05-web-scraping-md-readme > article > p:nth-child(49) > code:nth-child(1)"
  (plump:parse (dex:get "https://gist.github.com/masatoi/d37978c8cd21317b14d0cba258a74813")))
  0))

(clss:select
 ".hb2Smf"
 (plump:parse (dex:get "https://www.google.com/")))

;;; for site which require login

(dex:get "http://b.hatena.ne.jp/masatoi/hotentry") ;; => error

(defparameter *hatena-login-url* "https://www.hatena.ne.jp/login")
(defparameter *cookie-jar* (cl-cookie:make-cookie-jar))

(dex:post *hatena-login-url*
          :cookie-jar *cookie-jar*
          :content '(("name" . "masatoi")
                     ("password" . "hogehoge")))

(dex:get "http://b.hatena.ne.jp/masatoi/hotentry" :cookie-jar *cookie-jar*)

(defparameter *hatena-logout-url* "https://www.hatena.ne.jp/logout")

(defmacro with-hatena ((cookie-jar) (&key id password) &body body)
  `(let ((,cookie-jar (cl-cookie:make-cookie-jar)))
     (dex:post *hatena-login-url*
               :cookie-jar ,cookie-jar
               :content `(("name" . ,,id)
                          ("password" . ,,password)))
     (unwind-protect
          (progn ,@body)
       (dex:post *hatena-logout-url* :cookie-jar ,cookie-jar))))

(with-hatena (cookie-jar) (:id "masatoi" :password "hogehoge")
  (dex:get "http://b.hatena.ne.jp/masatoi/hotentry" :cookie-jar cookie-jar))


;;; for cl-igo

;; cl-igoをロードする
(ql:quickload :igo)

;; 辞書を読み込む
(igo:load-tagger "~/igo/ipadic/")

;; 形態素解析を実行する
(igo:parse "庭には二羽にわとりがいる。")

'(("庭" "名詞,一般,*,*,*,*,庭,ニワ,ニワ" 0)
  ("に" "助詞,格助詞,一般,*,*,*,に,ニ,ニ" 1)
  ("は" "助詞,係助詞,*,*,*,*,は,ハ,ワ" 2)
  ("二" "名詞,数,*,*,*,*,二,ニ,ニ" 3)
  ("羽" "名詞,接尾,助数詞,*,*,*,羽,ワ,ワ" 4)
  ("にわとり" "名詞,一般,*,*,*,*,にわとり,ニワトリ,ニワトリ" 5)
  ("が" "助詞,格助詞,一般,*,*,*,が,ガ,ガ" 9)
  ("いる" "動詞,自立,*,*,一段,基本形,いる,イル,イル" 10)
  ("。" "記号,句点,*,*,*,*,。,。,。" 12))

(igo:parse "すもももももももものうち。")

'(("すもも" "名詞,一般,*,*,*,*,すもも,スモモ,スモモ" 0)
  ("も" "助詞,係助詞,*,*,*,*,も,モ,モ" 3)
  ("もも" "名詞,一般,*,*,*,*,もも,モモ,モモ" 4)
  ("も" "助詞,係助詞,*,*,*,*,も,モ,モ" 6)
  ("もも" "名詞,一般,*,*,*,*,もも,モモ,モモ" 7)
  ("の" "助詞,連体化,*,*,*,*,の,ノ,ノ" 9)
  ("うち" "名詞,非自立,副詞可能,*,*,*,うち,ウチ,ウチ" 10)
  ("。" "記号,句点,*,*,*,*,。,。,。" 12))

;;; cl-docclass

(ql:quickload :cl-docclass)

(defparameter *livedoor-data-dir* #P"~/datasets/livedoor/text/")

(defparameter *news-site-names*
  '("kaden-channel" "peachy" "sports-watch" "dokujo-tsushin" "livedoor-homme"
    "topic-news" "it-life-hack" "movie-enter" "smax"))

(defparameter *livedoor-data*
  (mapcar (lambda (p)
            (uiop:directory-files
             (merge-pathnames (concatenate 'string p "/") *livedoor-data-dir*)))
          *news-site-names*))

(defparameter *livedoor-data-files*
  (alexandria:flatten *livedoor-data*))

;; 単語の文字列をキー、単語の通しインデックスと出現回数のドット対をバリューとするハッシュテーブル
(defparameter *word-hash* (make-hash-table :test 'equal))

;; *word-hash*に値を設定する(数秒かかる)
(dolist (file *livedoor-data-files*)
  (docclass:add-words-to-hash-from-file! file *word-hash*))

;; 頻度の小さい単語を除いてハッシュテーブルを作り直す
(setf *word-hash* (docclass:remove-infrequent-words *word-hash* 10))

;; テキストファイルのリストと*word-hash*から疎ベクトルのリストを作る(十数秒かかる)
(defparameter *text-sparse-vectors*
  (docclass:make-tf-idf-list-from-files *livedoor-data-files* *word-hash*))

(length *text-sparse-vectors*) ; => 7367 (文書数)
(hash-table-count *word-hash*) ; => 18372 (単語数)

(car *text-sparse-vectors*)
;; #S(CL-ONLINE-LEARNING.VECTOR::SPARSE-VECTOR
;;    :LENGTH 114
;;    :INDEX-VECTOR #(0 12 13 ...)
;;    :VALUE-VECTOR #(0.021566820323196546d0 0.02112624939291653d0 0.027720820546932805d0 ...))

;; 各テキストファイルの入っているディレクトリからクラスラベルをつける
(defparameter *class-labels*
  (alexandria:flatten
   (loop for class-id from 0 to (1- (length *livedoor-data*))
         for dir-length in (mapcar #'length *livedoor-data*)
         collect (make-list dir-length :initial-element class-id))))

(defparameter *dataset*
  (alexandria:shuffle (mapcar #'cons *class-labels* *text-sparse-vectors*)))

(defparameter *train-set* (subseq *dataset* 0 6367))
(defparameter *test-set* (nthcdr 6367 *dataset*))

(ql:quickload :cl-online-learning)

(defparameter *learner* (clol:make-one-vs-one 18372 9 'sparse-arow 0.1d0))
(defparameter *learner* (clol:make-one-vs-rest 18372 9 'sparse-arow 0.1d0))

(time (clol:train *learner* *train-set*))
;; Evaluation took:
;;   0.073 seconds of real time
;;   0.072121 seconds of total run time (0.072117 user, 0.000004 system)
;;   98.63% CPU
;;   143,653,754 processor cycles
;;   425,984 bytes consed

(clol:test *learner* *train-set*)
;; Accuracy: 99.79582%, Correct: 6354, Total: 6367
(clol:test *learner* *test-set*)
;; Accuracy: 95.700005%, Correct: 957, Total: 1000

(clol:one-vs-rest-predict *learner* (cdr (car *test-set*)))

(dotimes (i 10)
  (clol:train *learner* *train-set*)
  (format t "cycle: ~2D (train) " i)
  (clol:test *learner* *train-set*)
  (format t "cycle: ~2D (test)  " i)
  (clol:test *learner* *test-set*))

(defparameter result0.001
  (loop repeat 50
        collect (progn
                  (clol:train *learner* *train-set*)
                  (cons (clol:test *learner* *train-set*)
                        (clol:test *learner* *test-set*)))))

(ql:quickload :clgplot)
(clgp:plots (list (mapcar #'car result0.01)
                  (mapcar #'cdr result0.01)
                  (mapcar #'car result0.1)
                  (mapcar #'cdr result0.1)
                  (mapcar #'car result1)
                  (mapcar #'cdr result1)
                  (mapcar #'car result10)
                  (mapcar #'cdr result10))
            :y-range '(80 105))

'((89.853935 . 86.7) (92.68101 . 88.9) (94.565735 . 91.399994)
 (95.790794 . 92.299995) (96.544685 . 93.0) (97.15722 . 93.3)
 (97.534164 . 94.0) (98.05246 . 94.2) (98.30376 . 94.4) (98.6807 . 94.4)
 (98.88487 . 94.4) (99.04193 . 94.6) (99.23041 . 94.6) (99.324646 . 94.700005)
 (99.41888 . 94.700005) (99.497406 . 94.700005) (99.544525 . 94.8)
 (99.591644 . 94.8) (99.63876 . 95.0) (99.685875 . 95.0) (99.71729 . 95.1)
 (99.7487 . 95.1) (99.76441 . 95.1) (99.78012 . 95.200005)
 (99.78012 . 95.200005) (99.79582 . 95.200005) (99.82723 . 95.200005)
 (99.85865 . 95.200005) (99.89006 . 95.200005) (99.89006 . 95.200005)
 (99.90576 . 95.200005) (99.92147 . 95.200005) (99.93718 . 95.200005)
 (99.95288 . 95.200005) (99.96859 . 95.3) (99.96859 . 95.3) (99.96859 . 95.3)
 (99.96859 . 95.4) (99.96859 . 95.4) (99.96859 . 95.4) (99.98429 . 95.200005)
 (99.98429 . 95.200005) (99.98429 . 95.200005) (99.98429 . 95.3)
 (99.98429 . 95.3) (99.98429 . 95.3) (99.98429 . 95.3) (99.98429 . 95.3)
 (99.98429 . 95.3) (99.98429 . 95.3))
