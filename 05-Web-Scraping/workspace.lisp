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

"#tsf > div:nth-child(2) > div > div.RNNXgb > div > div.dRYYxd > div > span"


copy selector
"#file-05-web-scraping-md-readme > article > p:nth-child(49) > code:nth-child(1)"