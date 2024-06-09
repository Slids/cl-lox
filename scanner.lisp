(in-package :lox)

(defstruct scanner
  (stream nil :type stream)
  (tokens nil :type list)
  (line 1 :type fixnum))

(defmethod scan-tokens ((scanner scanner))
  (loop for next-char = (peek-char nil (scanner-stream scanner) nil)
	while next-char
	do
	   (scan-token scanner))
  
  (add-token scanner :eof "")
  (reverse (scanner-tokens scanner)))

(defmethod scan-token ((scanner scanner))
  (let ((next-char (read-char (scanner-stream scanner) nil)))
    (case next-char
      (#\( (add-token scanner :left-paren next-char))
      (#\) (add-token scanner :right-paren next-char))
      (#\{ (add-token scanner :left-brace next-char))
      (#\} (add-token scanner :right-brace next-char))
      (#\, (add-token scanner :comma next-char))
      (#\. (add-token scanner :dot next-char))
      (#\- (add-token scanner :minus next-char))
      (#\+ (add-token scanner :plus next-char))
      (#\; (add-token scanner :semicolon next-char))
      (#\* (add-token scanner :star next-char))
      (#\! (if (scanner-match scanner #\=)
	       (add-token scanner :bang-equal "!=")
	       (add-token scanner :bang next-char)))
      (#\= (if (scanner-match scanner #\=)
	       (add-token scanner :equal-equal "==")
	       (add-token scanner :equal next-char)))
      (#\< (if (scanner-match scanner #\= )
	       (add-token scanner :less-equal "<=")
	       (add-token scanner :less next-char)))
      (#\> (if (scanner-match scanner #\=)
	       (add-token scanner :greater-equal ">=")
	       (add-token scanner :greater next-char)))
      (#\/ (if (scanner-match scanner #\/)
	       (loop for c = (peek-char nil (scanner-stream scanner) nil)
		     while (and c (not (eq c #\newline)))  do 
		       (read-char (scanner-stream scanner)))
	       (add-token scanner :slash next-char)))
      ((#\space #\tab #\return))
      ((#\newline #\linefeed) (incf (scanner-line scanner)))
      (#\" (get-string scanner))
      (otherwise
       (cond ((digit-char-p next-char)
	      (unread-char next-char (scanner-stream scanner))
	      (get-number scanner))
	     ((alpha-char-p next-char)
	      (unread-char next-char (scanner-stream scanner))
	      (get-identifier scanner))
	     (t (lox-error (scanner-line scanner)
			   "Unexpected charecter.")))))))

(defun get-string (scanner)
  (let* ((char-list
	   (loop with stream = (scanner-stream scanner)
		 for next-char = (peek-char nil stream nil) do
		   (cond ((eq next-char #\n)
			  (incf (scanner-line scanner)))
			 ((not next-char)
			  (lox-error (scanner-line scanner)
				     "Unterminated string.")
			  (return-from get-string)))
		   (read-char stream)
		 until (eq next-char #\")
		 collect next-char))
	 (string (concatenate 'string char-list)))
    (add-token scanner :string (concatenate 'string "\"" string "\"") string)))

(defun peek-next (stream)
  (let ((first-char (peek-char nil stream nil)))
    (unless first-char
      (return-from peek-next))
    (read-char stream)
    (let ((second-char (peek-char nil stream nil)))
      (unread-char first-char stream)
      second-char)))

(defun get-number (scanner)
  (let* ((stream (scanner-stream scanner))
	 (chars-before-.
	   (loop for char = (peek-char nil stream nil) 
		 while (and char (digit-char-p char)) do
		   (read-char stream)
		 collect char))
	 (found-.
	   (and (eq (peek-char nil stream nil) #\.)
		(digit-char-p (or (peek-next stream)
				  #\Q))))
	 (chars-after-.
	   (and found-.
		(read-char stream)
		(loop for char = (peek-char nil stream nil) 
		      while (and char (digit-char-p char)) do
			(read-char stream)
		      collect char)))
	 (full-string (concatenate 'string
				   (concatenate 'string chars-before-.)
				   (when found-. ".")
				   (concatenate 'string chars-after-.)))
	 (num (read-from-string full-string)))
    (add-token scanner :number full-string (coerce num 'double-float))))


(let ((table (make-hash-table :test 'equal)))
  (setf (gethash "and" table) :and
	(gethash "class" table) :class
	(gethash "else" table) :else
	(gethash "false" table) :false
	(gethash "for" table) :for
	(gethash "fun" table) :fun
	(gethash "if" table) :if
	(gethash "nil" table) :nil
	(gethash "or" table) :or
	(gethash "print" table) :print
	(gethash "return" table) :return
	(gethash "super" table) :super
	(gethash "this" table) :this
	(gethash "true" table) :true
	(gethash "var" table) :var
	(gethash "while" table) :while)
  (defun get-identifier (scanner)
    (let* ((stream (scanner-stream scanner))
	   (identifier-list (loop for char = (peek-char nil stream nil) 
				  while (and char (alphanumericp char)) do
				    (read-char stream)
				  collect char))
	   (identifier (concatenate 'string identifier-list)))

      (let ((type (gethash identifier table)))
	(if type
	    (add-token scanner type identifier)
	    (add-token scanner :identifier identifier))))))

(defmethod scanner-match (scanner expected)
  (let ((peek-char (peek-char nil (scanner-stream scanner) nil)))
    (unless (and peek-char
		 (eq expected peek-char))
      (return-from scanner-match nil))
    (read-char (scanner-stream scanner))))

(defun add-token (scanner type text
		  &optional literal)
  (declare (type lox.token-type:token-type type)
	   (type scanner scanner))
  (push (lox.token:make-token
	 :type type
	 :lexeme
	 (if (characterp text)
	     (string text)
	     text)
	 :literal literal
	 :line (scanner-line scanner))
	(scanner-tokens scanner)))


