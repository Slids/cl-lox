(in-package #:lox)

(defstruct expr)

(defstruct (assign (:include expr))
  (name nil :type lox.token:token)
  (value nil :type expr))

(defstruct (binary (:include expr))
  (left nil :type expr)
  (operator nil :type lox.token:token)
  (right nil :type expr))

(defstruct (grouping (:include expr))
  (expression nil :type expr))

(defstruct (literal (:include expr))
  (value nil))

(defstruct (unary (:include expr))
  (operator nil :type lox.token:token)
  (right nil :type expr))

(defstruct (lox-variable (:include expr))
  (name nil :type lox.token:token))
