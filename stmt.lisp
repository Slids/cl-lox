(in-package #:lox)

(defstruct stmt)

(defstruct (expression (:include stmt))
  (expression nil :type expr))

(defstruct (lox-print (:include stmt))
  (expression nil :type expr))

(defstruct (var (:include stmt))
  (name nil :type lox.token:token)
  (initializer nil :type (or nil expr)))
