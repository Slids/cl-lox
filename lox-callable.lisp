(in-package #:lox)

(defstruct lox-callable
  (call)
  (to-string "" :type string)
  (arity nil :type (integer 0 255)))

(defmethod call ((callable lox-callable))
  (funcall (lox-callable-call callable)))

(defmethod print-object ((callable lox-callable) stream)
  (declare (stream stream))
  (format stream "<fn ~S>" (lox-callable-to-string callable)))

(defmethod arity ((callable lox-callable))
  (lox-callable-arity callable))
