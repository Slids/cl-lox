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
      '(("assign" (("lox.token:token" "name") ("expr" "value")))
	("binary" (("expr" "left") ("lox.token:token" "operator") ("expr" "right")))
	("call" (("expr" "callee") ("lox.token:token" "paren") ("arguments")))
	("grouping" (("expr" "expression")))
	("literal" (("value")))
	("logical" (("expr" "left") ("lox.token:token" "operator") ("expr" "right")))
	("unary" (("lox.token:token" "operator") ("expr" "right")))
	("lox-variable" (("lox.token:token" "name")))))
    (define-ast
	output-dir
      "stmt"
      '(("lox-block" (("statements")))
	("expression" (("expr" "expression")))
	("lox-function" (("lox.token:token" "name") ("params") ("body")))
	("lox-if" (("expr" "condition") ("stmt" "then-branch") ("(or null stmt)" "else-branch")))
	("lox-print" (("expr" "expression")))
	("lox-return" (("lox.token:token" "keyword") ("(or null expr)" "value")))
	("lox-while" (("expr" "condition") ("stmt" "body")))
	("var" (("lox.token:token" "name") ("(or nil expr)" "initializer")))))))

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
