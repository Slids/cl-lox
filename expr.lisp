(in-package #:lox)

(defstruct expr)

(defstruct (binary (:include expr))
  (left nil :type expr)
  (operator nil :type lox.token:token)
  (right nil :type expr))

(defstruct (grouping (:include expr))
  (expression nil :type expr))

(defstruct (literal (:include expr))
  (NIL nil :type value))

(defstruct (unary (:include expr))
  (operator nil :type lox.token)
  (right nil :type expr))
