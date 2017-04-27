#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :cliq/board
    (:documentation "Board implementation")
  (:use :cl)
  (:use :alexandria :com.andrewsoutar.commserver :dlist :hunchentoot)
  (:use :cliq/auth :cliq/page :cliq/thread :cliq/user)
  (:export #:board #:pages #:latest-page #:users #:summary
           #:board-page #:page-num #:num-threads #:threads))
(in-package :cliq/board)

(define-html-object board-page (authenticated-instance)
    ((board :initarg :board :reader board)
     (page-num :initarg :page-num :reader page-num)
     (num-threads :initform 0 :accessor num-threads)
     (threads :initform (make-dlist 0) :accessor threads)))
(defmethod auth-instance-to-user ((instance board-page) user)
  (auth-instance-to-user (board instance) user))

(define-authenticated-view board-page (board page-num threads)
  (:div
   (view board 'title)
   (if (= 0 (length threads))
       (:h3 :class :text-center "No threads")
       (view threads 'summary))
   (:nav
    (:ul :class :pager
         (when page-num
           (:li :class "previous"
                (:a
                 ;; FIXME
                 :href "/previous"
                 "← Newer")))
         (:li :class "next"
              (:a
               ;; FIXME
               :href "/next"
               "Older →"))))))
(define-authenticated-view (board-page summary) (threads)
  (:div :class "panel-body"
        (if (= 0 (length threads))
            (:b "No Threads")
            (loop for i from 0 below (min 5 (length threads))
                  do (view (elt threads i) 'linked-title)))))

(define-html-object board (authenticated-instance)
    ((name :initarg :name :reader name)
     (pages :initform (make-array 1 :adjustable t :fill-pointer 0) :accessor pages)
     (latest-page :reader latest-page)
     (thread-count :initform 0 :accessor thread-count)
     (creator :initarg :creator :reader creator)
     (users :initform (make-hash-table :test 'eq :synchronized t) :accessor users))
  (:weak nil))
(defmethod auth-instance-to-user ((instance board) user)
  (gethash user (users instance)))

(define-authenticated-view (board title) (name id)
  (:div :style "position: relative;"
        (:div :style "position: absolute; left: 0;"
              (:a :class "btn btn-default" :href "/" "Home"))
        (:div :style "position: absolute; right: 0;"
              (:a :class "btn btn-primary" :style "margin-right: 15px;"
                  :href (concatenate 'string "/thread/post?board="
                                     (princ-to-string id))
                  "Create Thread")
              (:a :class "btn btn-default" :href (concatenate 'string "/invite?board="
                                                              (princ-to-string id))
                  "Invite Someone"))
        (:h1 :class :text-center name)))
(define-authenticated-view (board summary) (name id latest-page)
  (:div :class "panel panel-default"
        (:div :class "panel-heading"
              (:a :href (concatenate 'string "/board?id=" (princ-to-string id))
                  name)
              (:div :class :clearfix))
        (view latest-page 'summary)))
(define-authenticated-view (board backlink) (name id)
  (:a :href (concatenate 'string "/board?id=" (princ-to-string id)) name))

(defmethod initialize-instance :after ((instance board) &key)
  (with-slots (creator users pages latest-page) instance
    (setf (gethash creator users) t)
    (vector-push-extend (make-instance 'board-page :board instance :page-num 0) pages)
    (setf latest-page (make-instance 'board-page :board instance :page-num nil))))

(define-easy-handler (/board :uri "/board" :default-request-type :get)
    (id (page :parameter-type 'integer))
  (if-let (board (gethash id (instances (find-class 'board))))
    (if-let (board-page (ignore-errors (aref (pages board) page)))
      (with-page board-page)
      (with-page (latest-page board)))
    (redirect "/")))
