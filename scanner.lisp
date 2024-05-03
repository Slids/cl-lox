(defpackage :lox.scanner
  (:use #:common-lisp))

(in-package :lox.scanner)

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
      (#\* (add-token scanner :star next-char)))))

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
  
  
