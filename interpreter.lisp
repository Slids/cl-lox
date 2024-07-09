(in-package #:lox)

(defvar *globals* (make-environment))
(defvar *environment* *globals*)

(env-define
 *globals*
 (lox.token:make-token :lexeme "clock" :type :identifier)
 (make-lox-callable
  :arity 0
  :call (lambda (args)
	  (declare (ignore args))
	  (coerce (get-universal-time) 'double-float))
  :to-string "native"))

(defun interpret (statements)
  ;; (ignore-errors
   (handler-bind
        ((runtime-error
    	  (lambda (c)
    	    (runtime-error c))))
     (dolist (statement statements)
       (accept statement)))) ;;)

(defun execute (statement)
  (accept statement))

(define-condition runtime-error (error)
  ((token :initarg :token
	  :accessor token)
   (message :initarg :message
	    :initform ""
	    :reader message))
  ;; the :report is the message into the debugger:
  (:report (lambda (condition stream)
     (format stream
             "~a~%"
             (message condition)))))

(defmethod evaluate ((expr literal))
  (literal-value expr))

(defmethod evaluate ((expr grouping))
  (evaluate (grouping-expression expr)))

(defmethod evaluate ((expr unary))
  (let ((right (evaluate (unary-right expr))))
    (check-number-operand (unary-operator expr)
			  right)
    (case (lox.token:token-type (unary-operator expr))
      (:minus (- right))
      (:bang (not (is-truthy right)))
      (otherwise nil))))

(defmethod evaluate ((expr lox-variable))
  (env-get *environment* (lox-variable-name expr)))

(defun check-number-operand (operator operand)
  (if (typep operand 'double-float)
      (values)
      (error 'runtime-error
	     :token operator
	     :message "Operand must be a number.")))

(defun check-number-operands (operator left right)
  (if (and (typep left 'double-float)
	   (typep right 'double-float))
      (values)
      (error 'runtime-error
	     :token operator
	     :message "Operands must be numbers.")))
      
  
(defun is-truthy (object) (not (not object)))

(defmethod evaluate ((expr binary))
  (let ((left (evaluate (binary-left expr)))
	(right (evaluate (binary-right expr)))
	(operator (binary-operator expr)))
    (case (lox.token:token-type operator)
      (:bang-equal (not (is-equal left right)))
      (:equal-equal (is-equal left right))
      (:greater
       (check-number-operands operator left right)
       (> left right))
      (:greater-equal 
       (check-number-operands operator left right)
       (>= left right))
      (:less 
       (check-number-operands operator left right)
       (< left right))
      (:less-equal 
       (check-number-operands operator left right)
       (<= left right))
      (:minus 
       (check-number-operands operator left right)
       (- left right))
      (:slash 
       (check-number-operands operator left right)
       (/ left right))
      (:star 
       (check-number-operands operator left right)
       (* left right))
      (:plus
       (cond ((and (numberp left) (numberp right))
	      (+ left right))
	     ((and (stringp left) (stringp right))
	      (concatenate 'string left right))
	     (t (error 'runtime-error
		       :token operator
		       :message "Operands must be two numbers or two strings."))))
      (otherwise nil))))

(defmethod evaluate ((expr call))
  (let ((callee (evaluate (call-callee expr)))
	arguments)
    (dolist (argument (call-arguments expr))
      (push (evaluate argument) arguments))
    (unless (= (length arguments) (arity callee))
      (error 'runtime-error
	     (call-paren expr)
	     (format nil "Expected ~a arguments but got ~a."
		     (arity callee)
		     (arity callee))))
	      
    (unless (typep callee 'lox-callable)
      (error 'runtime-error
	     (call-paren expr)
	     "Can only call functions and classes."))

    (funcall (lox-callable-call callee)
	     (nreverse arguments))))

(defun is-equal (left right)
  (cond ((and (null left) (null right)) t)
	((and (null left)) t)
	((and (stringp left) (stringp right))
	 (string= left right))
	((or (stringp left) (stringp right)) nil)
	(t (equalp left right))))

(defun stringify (object)
  (cond ((typep object 'double-float)
	 (let ((value (format nil "~0a" object)))
	   ;; print doubles that are integral as integers.
	   (if (string= (subseq value (- (length value) 4))
			".0d0")
	       (subseq value 0 (- (length value) 4))
	       value)))
	(t (format nil "~A" object))))

(defmethod accept ((stmt expression))
  (evaluate (expression-expression stmt))
  (values))

(defmethod accept ((stmt lox-function))
  (let ((function (lox-function stmt *environment*)))
    (env-define *environment*
		(lox-function-name stmt)
		function))
  (values))		      

(defmethod accept ((stmt lox-if))
  (cond ((is-truthy (evaluate (lox-if-condition stmt)))
	 (accept (lox-if-then-branch stmt)))
	((lox-if-else-branch stmt)
	 (accept (lox-if-else-branch stmt))))
  (values))

(defmethod accept ((stmt lox-print))
  (let ((value (evaluate (lox-print-expression stmt))))
    (format t "~A~%" (stringify value)))
  (values))

(defmethod accept ((stmt lox-return))
  (let (value)
    (when (lox-return-value stmt)
      (setf value
	    (evaluate (lox-return-value stmt))))
    (error 'inner-lox-return
	   :value value)))

(defmethod accept ((stmt var))
  (let ((value (and (var-initializer stmt)
		    (evaluate (var-initializer stmt)))))
    (env-define *environment* (var-name stmt) value))
  (values))

(defmethod accept ((stmt lox-while))
  (loop while (is-truthy (evaluate (lox-while-condition stmt))) do
    (accept (lox-while-body stmt)))
  (values))

(defmethod accept ((stmt lox-block))
  (let ((*environment* (make-environment :enclosing *environment*)))
    (dolist (statement (lox-block-statements stmt))
      (execute statement))))

	
(defmethod evaluate ((expr assign))
  (let ((value (evaluate (assign-value expr))))
    (env-assign *environment* (assign-name expr) value)
    value))

(defmethod evaluate ((expr logical))
  (let ((left (evaluate (logical-left expr))))
    (if (eql (lox.token:token-type (logical-operator expr)) :or)
	(if (is-truthy left)
	    (return-from evaluate left))
	(unless (is-truthy left)
	  (return-from evaluate left)))
    (evaluate (logical-right expr))))
