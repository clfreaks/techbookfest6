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
