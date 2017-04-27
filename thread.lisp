#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :cliq/thread
    (:documentation "Threads")
  (:use :cl)
  (:use :alexandria :cl-json :com.andrewsoutar.commserver :hunchentoot :split-sequence)
  (:import-from :ps #:defpsmacro #:chain)
  (:use :cliq/auth :cliq/page :cliq/post)
  (:export #:thread #:pages
           #:thread-page #:page-num #:posts
           #:summary #:backlink #:linked-title
           #:elist-length #:elist-pushend))
(in-package :cliq/thread)

(defstruct elist
  (head nil)
  (tail nil)
  (length 0))

(defun elist-push (thing elist)
  (with-slots (head tail length) elist
    (let ((cons (cons thing head)))
      (setf head cons)
      (unless tail (setf tail cons)))
    (incf length))
  elist)
(defun elist-pushend (thing elist)
  (with-slots (head tail length) elist
    (let ((cons (cons thing nil)))
      (if head
          (setf (cdr tail) cons)
          (setf head cons))
      (setf tail cons))
    (incf length))
  elist)

(defmethod encode-json ((object elist) &optional (stream nil streamp))
  (apply 'encode-json (elist-head object) (when streamp `(,stream))))

(defmethod render-view ((object elist) way stream)
  (render-view (elist-head object) way stream))

(define-html-object thread-page (authenticated-instance)
    ((thread :initarg :thread :reader thread)
     (page-num :initarg :page-num :reader page-num)
     (posts :initform (make-elist) :accessor posts)))
(defmethod auth-instance-to-user ((instance thread-page) user)
  (auth-instance-to-user (thread instance) user))

(define-authenticated-view thread-page (thread page-num posts)
  (:div
   (view thread 'header))
  (:div
   (view posts))
  (:form :action "/thread/post" :method :post
         (:div :class "well"
               (:div :class "form-group"
                     (:textarea :class "form-control" :placeholder "Reply" :name :body
                                :required t :rows 3))
               (:input :type :hidden :name :thread :value (id thread))
               (:input :type :hidden :name "_csrf" :value (get-csrf-token))
               (:button :type :submit :class "btn btn-primary btn-block" "Post")))
  (:nav
   (:ul :class :pager
        (:li :class (concatenate 'string "previous"
                                 (if (<= page-num 1)
                                     " disabled"
                                     ""))
             (:a
              ;; FIXME
              :href "/previous"
              "← Previous"))
        (:li :class "next"
             (:a
              ;; FIXME
              :href "/next"
              "Next →")))))

(define-html-object thread (authenticated-instance)
    ((board :initarg :board :reader board)
     (title :initarg :title :accessor title)
     (first-post :initarg :first-post :reader first-post)
     (pages :initform (make-array 0 :adjustable t :fill-pointer 0)
            :accessor pages))
  (:weak nil))
(defmethod auth-instance-to-user ((instance thread) user)
  (auth-instance-to-user (board instance) user))
(defmethod initialize-instance :after ((instance thread) &key)
  (let ((new-thread-page (make-instance 'thread-page :page-num 0 :thread instance)))
    (elist-pushend (first-post instance) (posts new-thread-page))
    (setf (thread (first-post instance)) instance)
    (vector-push-extend new-thread-page (pages instance))))

(defpsmacro princ-to-string (thing)
  `(chain ,thing (to-string)))
(define-authenticated-view (thread header) (title board)
  (:div :class "pull-right" (view board 'backlink))
  (:h1 :style "text-align: center;" title))
(define-authenticated-view (thread summary) (id title first-post)
  (:div :class "panel panel-default"
        (:div :class "panel-heading"
              (:a :href (concatenate 'string "/thread?id=" (princ-to-string id))
                  title)
              (:div :class :clearfix))
        (:div :class "panel-body"
              (view first-post 'summary))))
(define-authenticated-view (thread linked-title) (id title)
  (:h4 (:a :href (concatenate 'string "/thread?id=" (princ-to-string id)) title)))

(define-easy-handler (/thread :uri "/thread" :default-request-type :get)
    (id (page :parameter-type 'integer))
  (if-let (thread (gethash id (instances (find-class 'thread))))
    (if-let (thread-page (ignore-errors (aref (pages thread) page)))
      (with-page thread-page)
      (redirect (concatenate 'string (request-uri*) "&page=0")))
    (redirect "/")))
