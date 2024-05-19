(in-package #:lox)

;; Realistically this class is silly.
;; We should just be passing a list below, and have
;; current as a global...
;;
;; Again, trying to match the book.

(defstruct parser
  (tokens nil :type list)
  (current 0 :type integer))

(defun expression (parser)
  (declare (type parser parser))
  (equality parser))

;; Use a macro for binary parsers since there all the same form.
(defmacro binary-parser-impl (parser-name leaf-func token-types)
  `(defun ,parser-name (parser)
     (declare (type parser parser))
     (let ((expr (,leaf-func parser)))
       (loop while (match parser ,@token-types) do
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

(defun unary (parser)
  (declare (type parser parser))
  (if (match parser :bang :minus)
      (let ((operator (previous parser))
	    (right (unary parser)))
	(make-unary :operator operator
		    :right right))
      (primary parser)))

(defun primary (parser)
  (declare (type parser parser))
  (if (match parser :left-paren)
      (let ((expr (expression parser)))
	(consume parser :right-paren "Expect ')' after expression")
	(make-grouping expression))
      (make-literal
       (cond ((match parser :false) nil)
	     ((match parser :true) t)
	     ((match parser :nil) nil)
	     ((match parser :number :string)
	      (lox.token:token-literal
	       (previous parser)))))))

(define-condition parser-error (error)
  ())

(defun consume (parser type message)
  (declare (type parser parser)
	   (type lox.token-type:token-type type)
	   (type string message))
  (cond ((eql (check parser type))
	 (advance parser))
	(t (lox-error (peek parser) message)
	   (error 'parser-error))))

;; Lox error specific to parser.
(defmethod lox-error ((token lox.token:token) (message string))
  (let ((line (lox.token:token-line token))
	(lexeme (lox.token:token-lexeme token))
	(where (if (eql (lox.token:token-type token) :eof)
		   " at end"
		   (conatenate 'string " at '" lexeme "'"))))
    (report line where message)))
	
(defun match (parser &rest tokens)
  (declare (type parser parser))
  (dolist (token tokens)
    (when (check parser token)
      (advance parser)
      (return-from match t))))

(defun check (parser token-type)
  (declare (type parser parser)
	   (type lox.token-type:token-type token-type))
  (and (not (is-at-end parser))
       (eql (peek parser) token-type)))

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

    
	
