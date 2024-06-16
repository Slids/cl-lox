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

(defun while-statement (parser)
  (declare (type parser parser))
  (consume parser :left-paren "Expect '(' after 'while'.")
  (let ((condition (expression parser))
	(body (progn (consume parser :right-paren "Expect ')' after condition.")
		     (statement parser))))
    (make-lox-while :condition condition :body body)))

(defun expression (parser)
  (declare (type parser parser))
  (assignment parser))

(defun assignment (parser)
  (declare (type parser parser))
  (let ((expr (lox-or parser)))
    (when (parser-match parser :equal)
      (let ((equals (previous parser))
	    (value (assignment parser)))
	(when (typep expr 'lox-variable)
	  (return-from assignment
	    (make-assign :name (lox-variable-name expr)
			 :value value)))
	
	(lox-error equals "Invalid assignment target.")))
    expr))
	
(defun lox-or (parser)
  (declare (type parser parser))
  (let ((expr (lox-and parser)))
    (loop while (parser-match parser :or) do
      (let ((operator (previous parser))
	    (right (lox-and parser)))
	(setf expr (make-logical :left expr
				 :operator operator
				 :right right))))
    expr))

(defun lox-and (parser)
  (declare (type parser parser))
  (let ((expr (equality parser)))
    (loop while (parser-match parser :and) do
      (let ((operator (previous parser))
	    (right (equality parser)))
	(setf expr (make-logical :left expr
				 :operator operator
				 :right right))))
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
  (cond ((parser-match parser :for)
	 (for-statement parser))
	((parser-match parser :if)
	 (if-statement parser))
	((parser-match parser :print)
	 (print-statement parser))
	((parser-match parser :while)
	 (while-statement parser))
	((parser-match parser :left-brace)
	 (make-lox-block :statements (lox-block parser)))
	(t (expression-statement parser))))

(defun for-statement (parser)
  (declare (type parser parser))
  (consume parser :left-paren "Expect `(` after `for`.")

  (let (initializer condition increment body)
    (cond ((parser-match parser :semicolon))
	  ((parser-match parser :var)
	   (setf initializer (var-declaration parser)))
	  (t (setf initializer (expression-statement parser))))

    (if (check parser :semicolon)
	(setf condition (make-literal :value t))
	(setf condition (expression parser)))

    (consume parser :semicolon "Expect `;` after loop condition.")

    (unless (check parser :right-paren)
      (setf increment (expression parser)))

    (consume parser :right-paren "Expect `)` after for clauses.")
    
    (setf body (statement parser))

    (when increment
      (setf body (make-lox-block :statements
				 (list body (make-expression :expression increment)))))

    (setf body (make-lox-while :condition condition
			       :body body))

    (when initializer
      (setf body (make-lox-block :statements
				 (list initializer body))))

    body))

(defun if-statement (parser)
  (declare (type parser parser))
  (consume parser :left-paren "Expect `(` after `if`.")
  (let ((condition (expression parser)))
    (consume parser :right-paren "Expect `)` after condition.")
    (let ((then-branch (statement parser))
	  (else-branch (when (parser-match parser :else)
			 (statement parser))))
      (make-lox-if :condition condition
		   :then-branch then-branch
		   :else-branch else-branch))))

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

(defun lox-block (parser)
  (declare (type parser parser))
  (let (statements)
    (loop while (and (not (check parser :right-brace))
		     (not (is-at-end parser)))
	  do
	     (push (lox-declaration parser) statements))
    
    (consume parser :right-brace "Expect } after block.")
    (nreverse statements)))

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

    
	
