(in-package #:lox)

(defstruct environment
  (values (make-hash-table :test #'equalp) :type  hash-table))

(defun env-define (environment name value)
  (setf (gethash name (environment-values environment)) value))

(defun env-get (environment name)
  (let ((value (gethash name (environment-values environment))))
    (if value
	value
	(error 'runtime-error
	       :message
	       (format nil "Undefined variable '~a'." name)))))
