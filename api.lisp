#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :cliq/api
    (:documentation "The (possibly temporary) REST API")
  (:use :cl)
  (:use :alexandria :com.andrewsoutar.commserver :dlist :hunchentoot :split-sequence)
  (:use :cliq/auth :cliq/board :cliq/thread :cliq/page :cliq/post :cliq/user)
  (:export))
(in-package :cliq/api)

(defmacro bind-or-else ((var form) &optional else &body then)
  `(let ((,var ,form))
     (if ,var
         (progn ,@then)
         ,else)))

(defgeneric actually-post-to (place body user title))

(defun post-to (type place body &optional title)
  (bind-or-else (body (split-sequence #\Newline body :test 'char= :remove-empty-subseqs t))
                (redirect "/")          ; FIXME
    (bind-or-else (place (gethash place (instances (find-class type))))
                  (redirect "/")
      (let ((user (auth-user-in-request)))
        (if (and user (auth-instance-to-user place user))
            (actually-post-to place body user title)
            (progn
              (setf (return-code*) +http-forbidden+)
              (abort-request-handler)))))))

(defmethod actually-post-to ((board board) body user title)
  (when (or (null title) (zerop (length title)))
    (redirect "/")                      ; FIXME
    )
  (let* ((new-post (make-instance 'post :author user :body body))
         (new-thread (make-instance 'thread :board board :title title
                                            :first-post new-post)))
    (atomically (board
                 (page (aref (pages board) (1- (length (pages board)))))
                 (latest-page (latest-page board)))
      (when (= 100 (num-threads page))
        (setf page (make-instance 'board-page :page-num (1+ (page-num page)) :board board))
        (setf #1= (pages board)
              (let ((pages #1#))
                (vector-push-extend page pages)
                pages)))
      (push new-thread (threads page))
      (incf (num-threads page))
      (push new-thread (threads latest-page))
      (if (= 100 (num-threads latest-page))
          (dlist-pop (threads latest-page) :from-end t)
          (incf (num-threads latest-page))))
    (redirect (format nil "/thread?id=~D" (id new-thread)))))

(defmethod actually-post-to ((thread thread) body user title)
  (declare (ignore title))
  (let ((new-post (make-instance 'post :author user :body body :thread thread)))
    (let ((page-num
            (atomically (thread
                         (page (aref (pages thread) 0)))
              (if (= 100 (elist-length (posts page)))
                  (let ((new-page (make-instance 'thread-page :page-num (1+ (page-num page))
                                                              :thread thread)))
                    (elist-pushend new-post (posts new-page))
                    (setf #1= (pages thread)
                          (let ((pages #1#))
                            (vector-push-extend new-page pages)
                            pages)))
                  (setf (posts page) (elist-pushend new-post (posts page))))
              (page-num page))))
      (redirect (format nil "/thread?id=~D&page=~D" (id thread) page-num)))))

(define-html-object post-creation-page ()
    ((board :initarg :board)
     (thread :initarg :thread))
  (:private t))
(defview post-creation-page (board)
  (server-side-only ()
    (:h2 :style "text-align: center;" "Create a post:")
    (:form :style "padding: 15px;" :action "#" :method :post
           (:div :class "well"
                 (:div :class "form-group"
                       (:input :class "form-control"
                               :style "font-weight: bold; font-size: 22px; height: 2em;"
                               :type :text :placeholder "Title" :name :title :required t))
                 (:div :class "form-group"
                       (:textarea :class "form-control" :placeholder "Body" :name :body
                                  :required t :rows 15))
                 (:input :type :hidden :name :board :value board)
                 (:input :type :hidden :name "_csrf" :value (get-csrf-token))
                 (:div :class "form-group"
                       (:button :type :submit :class "btn btn-primary btn-block" "Post"))))))

(define-easy-handler (/thread/post :uri "/thread/post")
    (board thread (title :request-type :post) (body :request-type :post)
           (csrf :real-name "_csrf" :request-type :post))
  (cond ((null body)
         (with-page (make-instance 'post-creation-page :board board :thread thread)))
        ((not (check-csrf-token csrf))
         (setf (return-code*) +http-forbidden+)
         (abort-request-handler))
        (board (post-to 'board board body title))
        (thread (post-to 'thread thread body))
        (t (redirect "/"))))

(define-html-object board-creation-page ()
    ()
  (:private t))
(defview board-creation-page ()
  (server-side-only ()
    (:h2 :style "text-align: center;" "Create a board:")
    (:form :style "padding: 15px;" :action "#" :method :post
           (:div :class "well"
                 (:div :class "form-group"
                       (:input :class "form-control"
                               :style "font-weight: bold; font-size: 22px; height: 2em;"
                               :type :text :placeholder "Name" :name :name :required t))
                 (:input :type :hidden :name "_csrf" :value (get-csrf-token))
                 (:div :class "form-group"
                       (:button :type :submit :class "btn btn-primary btn-block" "Create"))))))

(define-easy-handler (/board/create :uri "/board/create")
    ((name :request-type :post) (csrf :real-name "_csrf" :request-type :post))
  (if-let ((user (auth-user-in-request)))
    (if (and name (plusp (length name))) ; FIXME should board names be unique?
        (if (check-csrf-token csrf)
            (let ((board (make-instance 'board :creator user :name name)))
              (atomically (user)
                (push board (boards user)))
              (redirect (format nil "/board?id=~D" (id board))))
            (progn
              (setf (return-code*) +http-forbidden+)
              (abort-request-handler)))
        (with-page (make-instance 'board-creation-page)))
    (progn
      (setf (return-code*) +http-forbidden+)
      (abort-request-handler))))
