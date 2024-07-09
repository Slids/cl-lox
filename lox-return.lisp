(in-package #:lox)

(define-condition inner-lox-return (error)
  ((value :initarg :value
	  :accessor value))
  (:report (lambda (condition stream)
	     (format stream
		     "inner-lox-return ~a~%"
		     (value condition)))))
