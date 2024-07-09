(in-package #:lox)

(defstruct (inner-lox-function (:include lox-callable)) 
  (declaration nil :type lox-function)
  (closure nil :type environment))

(defmethod lox-function ((decleration lox-function) (closure environment))
  (make-inner-lox-function
   :declaration decleration
   :closure closure
   :call
   (lambda (args)
     (handler-case 
	 (let ((*environment* (make-environment :enclosing closure)))
	   (loop for i from 0 below (length (lox-function-params decleration)) do
	     (env-define *environment*
			 (nth i (lox-function-params decleration))
			 (nth i args)))
	   (accept (lox-function-body decleration))
	   (values))
       (inner-lox-return (c)
	 (value c))))
   :arity (length (lox-function-params decleration))
   :to-string (lox.token:token-lexeme (lox-function-name decleration))))
       
	   
			  
