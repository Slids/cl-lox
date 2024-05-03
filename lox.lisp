(defpackage :lox
  (:use #:common-lisp))

(in-package :lox)

(defvar *had-error* nil
  "Has an error occured")

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
		  collect line)))
    (setf data (format nil "~{~a~}" data))
    (run data)

    ;; Indicate an error in the exit code.
    (when *had-error*
      #-swank
      (sb-ext:exit :code 65))))

(defun run-prompt ()
  (print "> ")
  (loop for line = (read-line)
	while line
	do
	   (run line)
	   (print "> ")))

(defun run (source)
  (declare (type string source))
  (with-input-from-string (stream source)
    (loop for next-char = (read-char stream nil)
	  while next-char do
	    (print next-char))))

(defun lox-error (line message)
  (declare (type string message)
	   (type fixnum line))
  (report line "" message))

(defun report (line where message)
  (declare (type string message where)
	   (type fixnum line))
  (setf *had-error* t)
  (format *error-output*
	  "[line ~a] Error ~a: ~a" line where message))

#-swank
(main (cdr sb-ext:*posix-argv*))
  
	   
