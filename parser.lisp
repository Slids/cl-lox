(in-package #:lox)

;; Realistically this class is silly.
;; We should just be passing a list below, and have
;; current as a global...
;;
;; Again, trying to match the book.

(defstruct parser
  (tokens nil :type list)
  (current 0 :type integer))

(defun parse (parser)
  (let (statements)
    (loop while (not (is-at-end parser)) do
      (push (lox-declaration parser) statements))
    (reverse statements)))

(defun lox-declaration (parser)
  (declare (type parser parser))
  (handler-bind
      ((parser-error
	 (lambda (err)
	   (declare (ignore err))
	   (synchronize parser)
	   (values))))
    (if (parser-match parser :var)
	(var-declaration parser)
	(statement parser))))

(defun var-declaration (parser)
  (declare (type parser parser))
  (let ((name (consume parser :identifier "Expect variable name."))
	(initializer
	  (and (parser-match parser :equal)
	       (expression parser))))
    (consume parser :semicolon "Expect `;` after declaration")
    (make-var :name name :initializer initializer)))

(defun expression (parser)
  (declare (type parser parser))
  (assignment parser))

(defun assignment (parser)
  (declare (type parser parser))
  (let ((expr (equality parser)))
    (when (parser-match parser :equal)
      (let ((equals (previous parser))
	    (value (assignment parser)))
	(when (typep expr 'lox-variable)
	  (return-from assignment
	    (make-assign :name (lox-variable-name expr)
			 :value value)))
	
	(lox-error equals "Invalid assignment target.")))
    expr))

	

;; Use a macro for binary parsers since there all the same form.
(defmacro binary-parser-impl (parser-name leaf-func token-types)
  `(defun ,parser-name (parser)
     (declare (type parser parser))
     (let ((expr (,leaf-func parser)))
       (loop while (parser-match parser ,@token-types) do
	 (let ((operator (previous parser))
	       (right (,leaf-func parser)))
	   (setf expr (make-binary :left expr
				   :operator operator
				   :right right))))
       expr)))

(binary-parser-impl equality comparison (:bang-equal :equal-equal))
(binary-parser-impl comparison term (:greater :greater-equal :less :less-equal))
(binary-parser-impl term factor (:minus :plus))
(binary-parser-impl factor unary (:slash :star))

(defun statement (parser)
  (declare (type parser parser))
  (if (parser-match parser :print)
      (print-statement parser)
      (expression-statement parser)))

(defun print-statement (parser)
  (declare (type parser parser))
  (let ((expr (expression parser)))
    (consume parser :semicolon "Expect `;` after value")
    (make-lox-print :expression expr)))

(defun expression-statement (parser)
  (declare (type parser parser))
  (let ((expr (expression parser)))
    (consume parser :semicolon "Expect `;` after expression")
    (make-expression :expression expr)))

(defun unary (parser)
  (declare (type parser parser))
  (if (parser-match parser :bang :minus)
      (let ((operator (previous parser))
	    (right (unary parser)))
	(make-unary :operator operator
		    :right right))
      (primary parser)))

(defun primary (parser)
  (declare (type parser parser))
  (cond ((parser-match parser :false)
	 (make-literal :value nil))
	((parser-match parser :true)
	 (make-literal :value  t))
	((parser-match parser  :nil)
	 (make-literal :value  nil))
	((parser-match parser :number :string)
	 (make-literal :value  (lox.token:token-literal
				(previous parser))))
	((parser-match parser :identifier)
	 (make-lox-variable :name (previous parser)))
	((parser-match parser :left-paren)
	 (let ((expr (expression parser)))
	   (consume parser :right-paren "Expect ')' after expression")
	   (make-grouping :expression expr)))
	(t
	 (lox-error (peek parser) "Expect expression."))))

(define-condition parser-error (error)
  ())

(defun consume (parser type message)
  (declare (type parser parser)
	   (type lox.token-type:token-type type)
	   (type string message))
  (cond ((check parser type)
	 (advance parser))
	(t (lox-error (peek parser) message)
	   (error 'parser-error))))

;; Lox error specific to parser.
(defmethod lox-error ((token lox.token:token) (message string))
  (let* ((line (lox.token:token-line token))
	 (lexeme (lox.token:token-lexeme token))
	 (where (if (eql (lox.token:token-type token) :eof)
		    " at end"
		    (concatenate 'string " at '" lexeme "'"))))
    (report line where message)))
	
(defun synchronize (parser)
  (declare (type parser parser))
  (loop while (not (is-at-end parser)) do
    (when (eql (lox.token:token-type (previous parser))
	       :semicolon)
      (return-from synchronize))

    (when (member (lox.token:token-type (peek parser))
		  '(:class :for :fun :if :print :return :var :while))
      (return-from synchronize))

    (advance parser)))
	

(defun parser-match (parser &rest tokens)
  (declare (type parser parser))
  (dolist (token tokens)
    (when (check parser token)
      (advance parser)
      (return-from parser-match t))))

(defun check (parser token-type)
  (declare (type parser parser)
	   (type lox.token-type:token-type token-type))
  (and (not (is-at-end parser))
       (eql (lox.token:token-type (peek parser)) token-type)))

(defun advance (parser)
  (declare (type parser parser))
  (unless (is-at-end parser)
    (setf (parser-current parser)
	  (1+ (parser-current parser))))
  (previous parser))

(defun is-at-end (parser)
  (declare (type parser parser))
  (eql (lox.token:token-type (peek parser))
       :eof))

(defun peek (parser)
  (declare (type parser parser))
  (nth (parser-current parser)
       (parser-tokens parser)))

(defun previous (parser)
  (declare (type parser parser))
  (nth (1- (parser-current parser))
       (parser-tokens parser)))

    
	
