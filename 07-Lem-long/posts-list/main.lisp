(defpackage #:lem-posts-list
  (:nicknames #:lem-posts-list/main)
  (:import-from #:trivial-open-browser
                #:open-browser)
  (:use #:cl
        #:lem
        #:lem-posts-list/posts))
(in-package #:lem-posts-list)

(define-major-mode posts-list-mode nil
    (:name "Posts list"
     :keymap *posts-list-mode-keymap*)
  (setf (buffer-read-only-p (current-buffer)) t))

(define-key *posts-list-mode-keymap* "Return" 'posts-list-select)
(define-key *posts-list-mode-keymap* "n" 'next-line)
(define-key *posts-list-mode-keymap* "p" 'previous-line)

(define-attribute author-attribute
  (t :foreground "red"))

(define-attribute title-attribute
  (:light :foreground "blue")
  (:dark :foreground "cyan"))

(defun write-post (point post)
  (with-point ((start point :right-inserting))
    (insert-string point
                   (format nil "[~A]" (post-author post))
                   :attribute 'author-attribute)
    (insert-string point " ")
    (insert-string point
                   (post-title post)
                   :attribute 'title-attribute)
    (insert-character point #\newline)
    (put-text-property start point :post post)))

(defun write-posts (point posts)
  (dolist (post posts)
    (write-post point post)))

(defun make-posts-list-buffer (subreddit)
  (let ((posts (fetch-posts subreddit))
        (buffer (make-buffer (format nil "*Reddit ~A*" subreddit))))
    (with-buffer-read-only buffer nil
      (erase-buffer buffer)
      (let ((point (buffer-point buffer)))
        (write-posts point posts)
        (buffer-start point)))
    buffer))

(define-command posts-list (subreddit) ("sSubreddit: ")
  (let ((buffer (make-posts-list-buffer subreddit)))
    (switch-to-buffer buffer)
    (change-buffer-mode buffer 'posts-list-mode)))

(define-command posts-list-select () ()
  (let ((post (text-property-at (current-point) :post)))
    (open-browser (post-url post))))
