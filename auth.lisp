#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :cliq/auth
    (:documentation "Login/logout")
  (:use :cl)
  (:shadowing-import-from :cl #:null)
  (:use :cl-base64 :hunchentoot :ironclad :com.andrewsoutar.commserver :split-sequence :simple-date-time :trivial-utf-8 :hunchensocket)
  (:import-from :com.andrewsoutar.commserver/websocket #:connection)
  (:use :cliq/page)
  (:export #:cliq-client #:user #:backend-user #:get-user
           #:authenticate #:make-user-auth-cookie #:auth-user-from-cookie
           #:authenticated-instance #:auth-user-in-request
           #:auth-instance-to-user #:define-authenticated-view))
(in-package :cliq/auth)

(defclass backend-user ()
  ((user :initarg :user :reader user)
   (digest-salt :initarg :digest-salt :accessor digest-salt)))
(defvar *users-by-id* (make-hash-table :test 'eql :synchronized t))
(defmethod initialize-instance :after ((instance backend-user) &key)
  (setf (gethash (id (user instance)) *users-by-id*) instance))

(defvar *users* (make-hash-table :test 'equal))

(defun get-user (username)
  (when (stringp username)
    (gethash (string-downcase username) *users*)))
(defun (setf get-user) (new-user username)
  (let ((new-user (if (typep new-user 'backend-user)
                      new-user
                      (destructuring-bind (fname lname username password) new-user
                        (make-instance 'backend-user
                                       :user (make-instance 'user :fname fname :lname lname
                                                                  :username username)
                                       :digest-salt (pbkdf2-hash-password-to-combined-string
                                                     (string-to-utf-8-bytes password)
                                                     :iterations 5000))))))
    (setf (gethash (string-downcase username) *users*) new-user)))

(defun authenticate (username password)
  (let ((user (get-user username)))
    (when (and user password
               (pbkdf2-check-password (string-to-utf-8-bytes password) (digest-salt user)))
      user)))

(defparameter *hmac-secret-key* (random-data 256))

(defun hmac (string)
  (let ((hmac (make-hmac *hmac-secret-key* :sha256)))
    (update-hmac hmac (string-to-utf-8-bytes string))
    (usb8-array-to-base64-string (hmac-digest hmac))))

(defun make-user-auth-cookie (user)
  (let ((suffix (concatenate 'string
                             (integer-to-base64-string (id (user user)))
                             ":" (integer-to-base64-string (serialize (hour+ (now) 24))))))
    (concatenate 'string (hmac suffix) ":" suffix)))

(defun auth-user-from-cookie (cookie)
  (ignore-errors
   (multiple-value-bind (hmac rest)
       (let ((pos (position #\: cookie :test 'char=)))
         (values (subseq cookie 0 pos) (subseq cookie (1+ pos))))
     (when (string= hmac (hmac rest))
       (destructuring-bind (b64-id b64-expiration) (split-sequence #\: rest :test 'char=)
         (when (date-time<= (now) (deserialize (base64-string-to-integer b64-expiration)))
           (user (gethash (base64-string-to-integer b64-id) *users-by-id*))))))))

;;; FIXME This shouldn't be here at all, except for the hack below... find a better home.
(defclass cliq-client (client)
  ((user :initform nil :accessor user)))

;;; FIXME FIXME UGLY HACK - this should be supported by commserver
(defmethod initialize-instance :after ((instance cliq-client) &key)
  (let ((cookie (cookie-in "auth" (client-request (connection instance)))))
    (when cookie
      (setf (user instance) (auth-user-from-cookie cookie)))))

(defgeneric auth-instance-to-user (instance user)
  (:method (instance user) nil))

(define-html-object authenticated-instance () () (:private t))

(defmethod authenticate-instance ((instance authenticated-instance) client)
  (let ((user (user client)))
    (and user (auth-instance-to-user instance user))))

(defun auth-user-in-request ()
  (setf (aux-request-value 'auth) (auth-user-from-cookie (cookie-in "auth"))))

(defmacro define-authenticated-view (object-and-name (&rest lambda-list) &body body)
  `(defview ,object-and-name ,lambda-list
     (server-side-only (this)
       (multiple-value-bind (user present-p)
           (aux-request-value 'auth)
         (unless present-p
           (setf user (auth-user-in-request)))
         (unless (and user (auth-instance-to-user this user))
           (setf (return-code*) +http-forbidden+)
           (abort-request-handler))))
     ,@body))
