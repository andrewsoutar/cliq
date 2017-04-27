#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :cliq
    (:documentation "An antisocial network")
  (:nicknames :cliq/main)
  (:use :cl)
  (:use :alexandria :cl-who)
  (:use :hunchentoot)
  (:use :hunchensocket)
  (:use :named-readtables)
  (:use :com.andrewsoutar.bootstrap)
  (:use :com.andrewsoutar.brace-lambda)
  (:use :com.andrewsoutar.commserver)
  ;; FIXME
  (:import-from :com.andrewsoutar.commserver/html-object-js #:compile-objects-to-ps)
  (:use
   :cliq/api :cliq/auth :cliq/board :cliq/login :cliq/page :cliq/post
   :cliq/register :cliq/thread :cliq/user)
  (:export #:main))
(in-package :cliq)

(in-readtable brace-lambda)

(defclass easy-ws-acceptor (websocket-acceptor easy-acceptor) ())

(defvar *api-handler*)

;;; Pre-populate the module and things at compile-time
(push {()
       (load "/home/user/.emacs.d/elpa/sly-20170205.1642/slynk/slynk-loader.lisp")
       (ql:quickload :slynk)
       (setf slynk::*module-loading-method* :slynk-loader)
       (slynk:slynk-add-load-paths
        (remove-duplicates
         '("/home/user/.emacs.d/elpa/sly-quicklisp-20170112.135/"
           "/home/user/.emacs.d/elpa/sly-named-readtables-20150817.816/"
           "/home/user/.emacs.d/elpa/sly-macrostep-20160119.434/"
           "/home/user/.emacs.d/elpa/sly-20170205.1642/contrib/"
           "/home/user/.emacs.d/elpa/sly-20170205.1642/contrib/"
           "/home/user/.emacs.d/elpa/sly-20170205.1642/contrib/"
           "/home/user/.emacs.d/elpa/sly-20170205.1642/contrib/"
           "/home/user/.emacs.d/elpa/sly-20170205.1642/contrib/"
           "/home/user/.emacs.d/elpa/sly-20170205.1642/contrib/"
           "/home/user/.emacs.d/elpa/sly-20170205.1642/contrib/")
         :test 'string=))
       ;; HACK
       (defvar slynk::*inspector-history* t)
       (slynk:slynk-require
        '("slynk-quicklisp" "slynk-named-readtables" "slynk-macrostep"
          "slynk-indentation" "slynk-stickers" "slynk-trace-dialog" "slynk-package-fu"
          "slynk-fancy-inspector" "slynk-arglists" "slynk-mrepl"))}
      uiop/image:*image-dump-hook*)
(defbootstrap slynk ()
  {()
   (ignore-errors (create-server :dont-close t))
   (values)}
  {()
   (values)})

(defbootstrap main (:after (slynk))
  {()
   (setf *api-handler* (make-instance 'commserver-resource :client 'cliq-client))
   (multiple-value-prog1
       (values (start (make-instance 'easy-ws-acceptor :name 'cliq
                                                       :address "0.0.0.0"
                                                       :port 8080)))
     (unless (progn #1= (get-user "god")) ; To make the indentation work
       (setf #1# '("Andrew" "Soutar" "god" "password"))))}
  {(acceptor)
   (stop acceptor :soft t)
   (setf *api-handler* nil)
   (values)})

(defvar *main-page*)

(define-html-object homepage ()
    ((user :initarg :user))
  (:private t))
(defview homepage (user)
  (server-side-only ()
    (:div :style "position: relative;"
          (:div :style "position: absolute; right: 0;"
                (:a :class "btn btn-default" :href "/board/create" "New Board"))
          (:h1 :class :text-center "Home"))
    (:div (view user 'homepage))))

;;; FIXME
(define-easy-handler (test :uri "/") ()
  (if-let ((user (auth-user-in-request)))
    (with-page (make-instance 'homepage :user user))
    (redirect "/login")))

(define-easy-handler (js :uri "/main.js") ()
  ;; FIXME
  (compile-objects-to-ps
   '(authenticated-instance page css js login-page
     board board-page thread thread-page post user)))

(defun find-ws-api (request)
  (when (string= "/api" (script-name request))
    *api-handler*))
(pushnew #'find-ws-api *websocket-dispatch-table*)
