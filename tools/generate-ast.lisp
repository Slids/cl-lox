(defpackage #:generate-ast
  (:use #:common-lisp))

(defun main (args)
  (unless (= (length args) 1)
    (print "Usage generate_ast <output directory>")
    #-swank
    (sb-ext:exit :code 64)
    (return-from main))

  (let ((output-dir (car args)))
    (define-ast
	output-dir
      "expr"
      '(("binary" (("expr" "left") ("lox.token:token" "operator") ("expr" "right")))
	("grouping" (("expr" "expression")))
	("literal" (("value")))
	("unary" (("lox.token" "operator") ("expr" "right")))))))

(defun define-ast (output-dir base-name types)
  (let ((path (concatenate 'string
			   output-dir
			   "/"
			   base-name
			   ".lisp")))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (format s "(in-package #:lox)~%~%(defstruct ~a)~%" base-name)
      
      (loop for type in types do
	(terpri s)
	(define-type s base-name (car type) (cadr type))))))

(defun define-type (stream base-type type fields)
  (format stream "(defstruct (~a (:include ~a))" type base-type)
  (loop for (type name) in fields do
    (terpri stream)
    (if name
	(format stream "  (~a nil :type ~a)" name type)
	(format stream "  (~a nil)" type)))
  (write-line ")" stream))

(main (cdr sb-ext:*posix-argv*))
