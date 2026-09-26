;;;; src/parser.lisp
(in-package #:yaml-kit)

(defun reader-emit (handler event)
  (funcall handler event))
