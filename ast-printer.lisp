(in-package #:lox)

;; To be near the books we probably should have tried
;; to shoe horn in a visitor pattern but
;; thats so far away from lisp I don't want to...

(defmethod print-expr ((e binary) &optional stream)
  (parenthesize stream
		(lox.token:token-lexeme (binary-operator e))
		(binary-left e)
		(binary-right e)))

(defmethod print-expr ((e grouping) &optional stream)
  (parenthesize stream
		"group"
		(grouping-expression e)))

(defmethod print-expr ((e literal) &optional stream)
  (format stream "~A"
	  (if (not (literal-value e))
	      "nil"
	      (literal-value e))))

(defmethod print-expr ((e unary) &optional stream)
  (parenthesize stream
		(lox.token:token-lexeme (unary-operator e))
		(unary-right e)))

(defun parenthesize (stream name &rest exprs)
  (format stream  "(~a" name)
  (dolist (e exprs)
    (format stream " ")
    (print-expr e stream))
  (format stream ")")
  (values))
