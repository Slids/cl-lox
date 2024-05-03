(defpackage :lox.token
  (:use #:common-lisp)
  (:export #:token
	   #:make-token))

(in-package :lox.token)

(defstruct token
  (type nil :type (or nil lox.token-type:token-type))
  (lexeme "" :type string)
  (literal nil)
  (line -1 :type fixnum))

(defmethod print-object ((obj token) out)
  (print-unreadable-object (obj out :type t)
    (format out "<~a ~a ~a>"
	    (token-type obj)
	    (token-lexeme obj)
	    (token-literal obj))))
