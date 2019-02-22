(defpackage #:lem-posts-list
  (:nicknames #:lem-posts-list/main)
  (:import-from #:jonathan)
  (:import-from #:dexador)
  (:import-from #:trivia
                #:match
                #:plist)
  (:use #:cl #:lem))
(in-package #:lem-posts-list)

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

(defun init ()
  (jojo:parse (dex:get "http://reddit.com/r/lisp/.json")))
