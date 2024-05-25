(defsystem :cl-lox
  :author "Jonathan Godbout <jgodbout@google.com>"
  :description "Lox in cl"
  :license "MIT"
  ;; :serial t
  :components ((:file "token-type")
	       (:file "token")
	       (:file "lox")
	       (:file "expr")
	       (:file "ast-printer")
	       (:file "scanner")
	       (:file "parser")
	       (:file "interpreter")))
