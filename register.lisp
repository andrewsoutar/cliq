#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :cliq/register
    (:documentation "Registration")
  (:use :cl)
  (:shadowing-import-from :cl #:null)
  (:use
   :alexandria :cl-base64 :com.andrewsoutar.brace-lambda :com.andrewsoutar.commserver :hunchentoot
   :ironclad :named-readtables :simple-date-time)
  (:import-from :sb-ext #:cas)
  (:use :cliq/auth :cliq/board :cliq/page :cliq/user)
  (:export #:registration-page))
(in-package :cliq/register)

(in-readtable brace-lambda)

(define-html-object registration-page ()
    ((invite :initarg :invite))
  (:private t))

(defview registration-page (invite)
  (:div :class "text-center"
        (:form :method :post :style "max-width: 375px; margin: 0 auto; padding: 15px;"
               (:div :class :well :style "padding: 40px;"
                     (:h1 :style "text-align: center; margin-top: 10px; margin-bottom: 30px;"
                          "Register")
                     (:input :type :text :class :form-control
                             :style "border-bottom-left-radius: 0; border-bottom-right-radius: 0; margin-bottom: -1px;"
                             :name :fname :placeholder "First Name"
                             :autofocus t :required t)
                     (loop for (type name placeholder) in
                           '((:text :lname "Last Name")
                             (:text :username "Username")
                             (:password :password "Password"))
                           do (:input :type type :class :form-control
                                      :style "border-radius: 0; margin-bottom: -1px;"
                                      :name name :placeholder placeholder :required t))
                     (:br)
                     (:input :type :hidden :name "_csrf" :value (get-csrf-token))
                     (:button :type :submit :class "btn btn-lg btn-primary btn-block"
                              "Register")))))

(defclass invite ()
  ((code :initform (usb8-array-to-base64-string (random-data 256)) :reader code)
   (inviter :initarg :inviter :reader inviter)
   (board :initarg :board :reader board)
   (expiration :initform (hour+ (now) 24) :reader expiration)
   (used :initform nil :reader used)))
(defvar *invites* (make-hash-table :test 'equal :synchronized t))
(defmethod initialize-instance :after ((instance invite) &key)
  (setf (gethash (code instance) *invites*) instance))

(defun validate-invite (invite-code)
  (when-let ((invite (gethash invite-code *invites*)))
    (and (date-time< (now) (expiration invite))
         (not (used invite))
         (gethash (inviter invite) (users (board invite)))
         invite)))

(defun invite-user (user invite)
  (if (eq (cas (slot-value invite 'used) nil t) nil)
      (progn
        (atomically (user
                     (board (board invite)))
          (setf (gethash user (users board)) t)
          (push board (boards user)))
        (redirect (format nil "/board?id=~D" (id (board invite)))))
      (with-page "Invalid or expired invite code"))) ;FIXME

(define-html-object invitation-page ()
    ((code :initarg :code))
  (:private t))
(defview invitation-page (code)
  (server-side-only ()
    (:div :class :well
          (:p "Send the other person the following invite link: "
              (let ((link (concatenate 'string "http://cliq.andrewsoutar.com:8080/register?invite=" (url-encode code)))) ;FIXME FIXME FIXME
                (:a :href link link))))))

(define-easy-handler (invite :uri "/invite" :default-request-type :get)
    (board)
  (let ((user (auth-user-in-request))
        (board (gethash board (instances (find-class 'board)))))
    (if (and user board (gethash user (users board)))
        (let ((invite (make-instance 'invite :inviter user :board board)))
          (with-page
            (make-instance 'invitation-page :code (code invite))))
        (progn
          (setf (return-code*) +http-forbidden+)
          (abort-request-handler)))))

;;; FIXME We need to do something to clear up this whole backend/frontend users thing
;;; FIXME FIXME We are performing state changes via a GET request here when a logged-in user
;;; clicks on a board. We should, at least, show some kind of confirmation page or something
;;; FIXME FIXME FIXME The above isn't just a theoretical or pedantic issue -
;;; it's causing actual bugs due to chrome preloading
(define-easy-handler (register :uri "/register" :default-request-type :post)
    ((invite :request-type :both) fname lname username password
     (csrf :real-name "_csrf"))
  (if invite
      (if-let (invite (validate-invite invite))
        (if-let (current-user (auth-user-in-request))
          (invite-user current-user invite)
          (if-let ((backend-user (authenticate username password)))
            (progn
              (set-cookie "auth" :value (make-user-auth-cookie backend-user))
              (invite-user (user backend-user) invite))
            (cond ((not (and fname lname username password))
                   (with-page (make-instance 'registration-page :invite invite)))
                  ((not (check-csrf-token csrf))
                   (setf (return-code*) +http-forbidden+)
                   (abort-request-handler))
                  ((notevery (lambda (x) (plusp (length x))) `(,fname ,lname ,username ,password)) ; For some reason, SBCL throws warnings for anything other than an explicit lambda here
                   (with-page (make-instance 'registration-page :invite invite))) ;FIXME show an error message
                  ((get-user username)
                   (with-page (make-instance 'registration-page :invite invite))) ;FIXME show an error message
                  (t
                   (setf (get-user username) `(,fname ,lname ,username ,password)) ;FIXME this syntax is unintuitive and against the spec
                   (let ((backend-user (get-user username)))
                     (set-cookie "auth" :value (make-user-auth-cookie backend-user))
                     (invite-user (user backend-user) invite))))))
        (with-page "Invalid or expired invite code")) ;FIXME
      (progn
        (setf (return-code*) +http-forbidden+)
        (abort-request-handler))))
