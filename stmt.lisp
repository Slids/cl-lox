(in-package #:lox)

(defstruct stmt)

(defstruct (lox-block (:include stmt))
  (statements nil))

(defstruct (expression (:include stmt))
  (expression nil :type expr))

(defstruct (lox-function (:include stmt))
  (name nil :type lox.token:token)
  (params nil)
  (body nil))

(defstruct (lox-if (:include stmt))
  (condition nil :type expr)
  (then-branch nil :type stmt)
  (else-branch nil :type (or null stmt)))

(defstruct (lox-print (:include stmt))
  (expression nil :type expr))

(defstruct (lox-return (:include stmt))
  (keyword nil :type lox.token:token)
  (value nil :type (or null expr)))

(defstruct (lox-while (:include stmt))
  (condition nil :type expr)
  (body nil :type stmt))

(defstruct (var (:include stmt))
  (name nil :type lox.token:token)
  (initializer nil :type (or nil expr)))
