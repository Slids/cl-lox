(in-package #:lox)

(defstruct environment
  (values (make-hash-table :test #'equalp) :type  hash-table)
  (enclosing nil :type (or null environment)))

(defun env-define (environment name value)
  (declare (type lox.token:token name)
	   (type environment environment))
  (setf (gethash (lox.token:token-lexeme name)
		 (environment-values environment))
	value))

(defun env-get (environment name)
  (declare (type lox.token:token name)
	   (type environment environment))
  (multiple-value-bind (value present-p)
      (gethash (lox.token:token-lexeme name)
	       (environment-values environment))
    (cond (present-p value)
	  ((environment-enclosing environment)
	   (env-get (environment-enclosing environment) name))
	  (t (error 'runtime-error
		    :token name
		    :message
		    (format nil "Undefined variable '~a'." name))))))

(defun env-assign (environment name value)
  (declare (type lox.token:token name)
	   (type environment environment))
  (multiple-value-bind (_ present-p)
      (gethash (lox.token:token-lexeme name)
	       (environment-values environment))
    (declare (ignore _))
    (cond (present-p
	   (setf (gethash (lox.token:token-lexeme name) (environment-values environment)) value)
	   (values))
	  ((environment-enclosing environment)
	   (env-assign (environment-enclosing environment) name value))
	  (t (error 'runtime-error
		    :token name
		    :message
		    (format nil "Undefined variable '~a'." name))))))
