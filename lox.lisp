(defpackage :lox
  (:use #:common-lisp)
  (:export #:lox-error))

(in-package :lox)

(defvar *had-error* nil
  "Has an error occured")
(defvar *had-runtime-error* nil
  "Has a runtime error occured")

(defun main (args)
  (cond ((> (length args) 1)
	 (print "Usage cl-lox: cl-lox [script]")
	 #-swank
	 (sb-ext:exit :code 64))
	((= (length args) 1)
	 (run-file (car args)))
	(t (run-prompt))))

;; This is silly, instead we should really just
;; pass the stream down and read it, reading it into memory
;; like this is wasteful. That being said, I'm trying to
;; follow the book, so take it as you may...
(defun run-file (path)
  (let (data)
    (with-open-file (stream path :direction :input)
      (setf data 
	    (loop for line = (read-line stream nil)
		  while line
		  collect line
		  collect #\newline)))
    (setf data (format nil "~{~a~}" data))
    (run data)

    ;; Indicate an error in the exit code.
    (when *had-error*
      #-swank
      (sb-ext:exit :code 65))
    (when *had-runtime-error*
      #-swank
      (sb-ext:exit :code 70))))

(defun run-prompt ()
  (print "> ")
  (loop for line = (read-line)
	while line
	do
	   (run line)
	   (print "> ")
	   (setf *had-error* nil)))

(defun run (source)
  (declare (type string source))
  (with-input-from-string (stream source)
    (let* ((scanner (make-scanner :stream stream))
	   (tokens (scan-tokens scanner))
	   (parser (make-parser :tokens tokens))
	   (statements (parse parser)))
      (unless *had-error*
	;; (print statements)
	(interpret statements)
	))))

(defun runtime-error (error)
  (format t "~S ~% [line ~A]~%"
	  (message error)
	  (lox.token:token-line (token error)))
  (setf *had-runtime-error* t))

(defmethod lox-error ((line fixnum) (message string))
  (report line "" message))

(defun report (line where message)
  (declare (type string message where)
	   (type fixnum line))
  (setf *had-error* t)
  (format *error-output*
	  "[line ~a] Error ~a: ~a~%" line where message))

#-swank
(main (cdr sb-ext:*posix-argv*))
  
	   
