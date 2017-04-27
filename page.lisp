#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :cliq/page
    (:documentation "The primary page template")
  (:use :cl)
  (:use :alexandria :com.andrewsoutar.brace-lambda :com.andrewsoutar.commserver :named-readtables)
  (:export #:css #:js #:page #:with-page))
(in-package :cliq/page)

(in-readtable brace-lambda)

(define-html-object css ()
    ((src :initarg :src :accessor src)
     (hash :initarg :hash :initform nil :accessor hash)
     (crossorigin :initarg :crossorigin :initform nil :accessor crossorigin)))
(defview css (src hash crossorigin)
  (:link :rel :stylesheet :href src :integrity hash :crossorigin crossorigin))

(define-html-object js ()
    ((src :initarg :src :accessor src)
     (hash :initarg :hash :initform nil :accessor hash)
     (crossorigin :initarg :crossorigin :initform nil :accessor crossorigin)))
(defview js (src hash crossorigin)
  (:script :src src :integrity hash :crossorigin crossorigin))

(defparameter *bootstrap-head*
  (mapcar {apply 'make-instance (if (search ".js" (car %1)) 'js 'css)
                 :src (concatenate 'string "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/"
                                   (car %1))
                 :crossorigin "anonymous"
                 (cdr %1)}
          '(("css/bootstrap.min.css"
             :hash "sha384-1q8mTJOASx8j1Au+a5WDVnPi2lkFfwwEAa8hDDdjZlpLegxhjVME1fgjWPGmkzs7")
            ("css/bootstrap-theme.min.css"
             :hash "sha384-fLW2N01lMqjakBkx3l/M9EahuwpSfeNvV63J5ezn3uZzapT0u7EYsXMjQV+0En5r")
            ("js/bootstrap.min.js"
             :hash "sha384-0mSbJDEHialfmuBBQP6A4Qrprq5OVfW37PRR3j5ELqxss1yVqOtnepnHVP9aJ7xS"))))
(defparameter *jquery-js*
  (make-instance 'js :src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js"))
(defparameter *main-js* (make-instance 'js :src "/main.js"))

(define-html-object page ()
    ((head :initarg :head :initform `(,*jquery-js* ,*bootstrap-head* ,*main-js*) :accessor head)
     (content :initarg :content :accessor content))
  (:private t))
(defview page (head content)
  (:html (:head head)
         (:body
          (:div :class "container"
                content))))

(defmacro with-page (&body content)
  (with-gensyms (stream)
    `(with-output-to-string (,stream)
       (write-string "<!DOCTYPE html>" ,stream)
       (with-html (,stream)
         (view (make-instance 'page :content (list ,@content)))))))
