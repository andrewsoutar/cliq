#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :cliq/login
    (:documentation "Login page")
  (:use :cl)
  (:use :com.andrewsoutar.commserver :hunchentoot)
  (:use :cliq/auth :cliq/page)
  (:export #:login-page))
(in-package :cliq/login)

(define-html-object login-page ()
    ((login-failure :initarg :login-failure
                    :initform nil
                    :accessor login-failure)))

(defview login-page (login-failure)
  (:div :class "text-center"
        (when login-failure
          (:div :class "alert alert-danger text-center" :style "display: inline-block;"
                (:strong "Incorrect username or password!")
                " Please double-check and try again."))
        (:form :method :post :style "max-width: 375px; margin: 0 auto; padding: 15px;"
               (:div :class :well :style "padding: 40px;"
                     (:h1 :style "text-align: center; margin-top: 10px; margin-bottom: 30px;"
                          "Login")
                     (:input :type :text :class :form-control
                             :style "border-bottom-left-radius: 0; border-bottom-right-radius: 0; margin-bottom: -1px;"
                             :name "username" :placeholder "Username"
                             :autofocus t :required t)
                     (:input :type :password :class :form-control
                             :style "border-top-left-radius: 0; border-top-right-radius: 0;"
                             :name "password" :placeholder "Password" :required t)
                     ;; FIXME FIXME FIXME CSRF token
                     (:br)
                     (:input :type :hidden :name "_csrf" :value (get-csrf-token))
                     (:button :type :submit :class "btn btn-lg btn-primary btn-block"
                              "Login")))))

(define-easy-handler (/login :uri "/login") (src username password
                                                 (csrf :real-name "_csrf" :request-type :post))
  (when (and username password)
    (if (check-csrf-token csrf)
        (let ((user (authenticate username password)))
          (when user
            (set-cookie "auth" :value (make-user-auth-cookie user))
            (redirect (or src "/"))
            (return-from /login)))
        (progn
          (setf (return-code*) +http-forbidden+)
          (abort-request-handler))      ; FIXME We use this so often, it should be a function somewhere
        ))
  (with-page
    (make-instance 'login-page :login-failure (when (and username password) t))))

(define-easy-handler (/logout :uri "/logout") ()
  (set-cookie "auth" :value "")
  (redirect "/"))
