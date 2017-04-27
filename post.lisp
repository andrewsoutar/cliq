#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :cliq/post
    (:documentation "Posts")
  (:use :cl)
  (:shadow #:client)
  (:use :com.andrewsoutar.commserver :hunchentoot :ps :simple-date-time :split-sequence)
  (:use :cliq/auth :cliq/user)
  (:export #:post #:thread
           #:summary))
(in-package :cliq/post)

(define-html-object post (authenticated-instance)
    ((thread :initarg :thread :accessor thread)
     (author :initarg :author :reader author)
     (timestamp :initform (|yyyy-mm-dd hh:mm| (now)))
     (body :initarg :body :accessor body)))
(defmethod auth-instance-to-user ((instance post) user)
  (auth-instance-to-user (thread instance) user))

(define-authenticated-view post (author timestamp body)
  (:div :class "panel panel-default"
        (:div :class "panel-heading"
              (:h4 (view author 'linked-name))
              (:div :class :clearfix))
        (:div :class "panel-body"
              (dolist (paragraph body)
                (:p paragraph))
              (:p :class "small text-muted" timestamp))))

;;; FIXME this doesn't belong here
(defpsmacro subseq (sequence start &optional end)
  `(chain ,sequence (slice ,start ,@ (when end `(,end)))))

(define-authenticated-view (post summary) (body)
  (:p (let ((first-paragraph (elt body 0)))
        (if (> (length first-paragraph) 1000)
            (concatenate 'string (subseq first-paragraph 0 977) "...")
            first-paragraph))))
