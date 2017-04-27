#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :cliq/user
    (:documentation "Client-side user models")
  (:use :cl)
  (:use :com.andrewsoutar.commserver)
  (:use :cliq/auth)
  (:export #:user #:fname #:lname #:username #:boards #:linked-name #:homepage #:summary))
(in-package :cliq/user)

(define-html-object user (authenticated-instance)
    ((fname :initarg :fname :accessor fname)
     (lname :initarg :lname :accessor lname)
     (username :initarg :username :reader username)
     (boards :initform () :accessor boards)))
(defmethod auth-instance-to-user ((instance user) user)
  (not (null user)))

(define-authenticated-view (user linked-name) (fname lname username)
  (:span
   (concatenate 'string fname " " lname " ")
   (:small username)))

(define-authenticated-view (user homepage) (boards)
  (:div
   (loop for board in boards
         do (view board 'summary))))
