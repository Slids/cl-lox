(defsystem :cl-lox
  :author "Jonathan Godbout <jgodbout@google.com>"
  :description "Lox in cl"
  :license "MIT"
  ;; :serial t
  :components ((:file "token-type")
	       (:file "token")
	       (:file "lox")
	       (:file "expr")
	       (:file "stmt")
	       (:file "environment")
	       (:file "lox-callable")
	       (:file "lox-function")
	       (:file "lox-return")
	       (:file "ast-printer")
	       (:file "scanner")
	       (:file "parser")
	       (:file "interpreter")))
