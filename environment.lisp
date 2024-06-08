(in-package #:lox)

(defstruct environment
  (values (make-hash-table :test #'equalp) :type  hash-table))

(defun env-define (environment name value)
  (declare (type lox.token:token name)
	   (type environment environment))
  (setf (gethash name (environment-values environment)) value))

(defun env-get (environment name)
  (declare (type lox.token:token name)
	   (type environment environment))
  (multiple-value-bind (value present-p)
      (gethash name (environment-values environment))
    (if present-p
	value
	(error 'runtime-error
	       :message
	       (format nil "Undefined variable '~a'." name)))))

(defun env-assign (environment name value)
  (declare (type lox.token:token name)
	   (type environment environment))
  (multiple-value-bind (_ present-p)
      (gethash name (environment-values environment))
    (declare (ignore _))
    (cond (present-p
	   (setf (gethash name (environment-values environment)) value)
	   (values))
	  (t
	   (error 'runtime-error
		  :message
		  (format nil "Undefined variable '~a'." name))))))
