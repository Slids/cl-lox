(defpackage :lox.token-type
  (:use #:common-lisp)
  (:export #:token-type))

(in-package :lox.token-type)

(deftype token-type ()
  '(member
    ;; Single-charecter Tokens
    :left-paren
    :right-paren
    :left-brace
    :right-brace
    :comma
    :dot
    :minus
    :plus
    :semicolon
    :slash
    :star

    ;; One or two charecter tokens
    :bang
    :bang-equal
    :equal
    :equal-equal
    :greater
    :greater-equal
    :less
    :less-equal

    ;; Literals
    :identifier
    :string
    :number

    ;; Keywords
    :and
    :class
    :else
    :false
    :fun
    :for
    :if
    :nil
    :or
    :print
    :return
    :super
    :this
    :true
    :var
    :while

    :eof))
