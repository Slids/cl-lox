(in-package #:lox)

(defstruct expr)

(defstruct (assign (:include expr))
  (name nil :type lox.token:token)
  (value nil :type expr))

(defstruct (binary (:include expr))
  (left nil :type expr)
  (operator nil :type lox.token:token)
  (right nil :type expr))

(defstruct (call (:include expr))
  (callee nil :type expr)
  (paren nil :type lox.token:token)
  (arguments nil))

(defstruct (grouping (:include expr))
  (expression nil :type expr))

(defstruct (literal (:include expr))
  (value nil))

(defstruct (logical (:include expr))
  (left nil :type expr)
  (operator nil :type lox.token:token)
  (right nil :type expr))

(defstruct (unary (:include expr))
  (operator nil :type lox.token:token)
  (right nil :type expr))

(defstruct (lox-variable (:include expr))
  (name nil :type lox.token:token))
