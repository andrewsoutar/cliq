#+named-readtables
(named-readtables:in-readtable :standard)

(uiop:define-package :cliq/serialize
    (:documentation "Serializing data to disk using fake FASLs")
  (:use :cl)
  (:export #:dump #:undump))
(in-package :cliq/serialize)

(defvar *data*)

(defparameter *data-dump-string*
  (let ((*package* (find-package :keyword)))
    (format nil "(setf ~S '#.~:*~S)" '*data*)))

(defun compile-fasl (fasl &optional (where "DUMP"))
  (let ((fasl-output (sb-fasl:open-fasl-output (pathname fasl) where)))
    (unwind-protect
         (let ((sb-c::*block-compile-arg* nil)
               (sb-c::*compiler-trace-output* nil)
               (sb-c::*compile-object* fasl-output)
               (sb-c::*compile-toplevel-object* nil))
           (with-input-from-string (stream *data-dump-string*)
             (sb-c::sub-compile-file (sb-c::make-source-info
                                      :stream stream
                                      :file-info (sb-c::make-file-info :name :lisp)))))
      (sb-fasl:close-fasl-output fasl-output nil))))

(defun dump (object fasl)
  (let ((*data* object))
    (compile-fasl fasl)))

(defun undump (fasl)
  (let (*data*)
    (load fasl)
    *data*))
