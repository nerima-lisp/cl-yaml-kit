;;;; src/char-classes.lisp
(in-package #:yaml-kit)

(defun yaml-line-break-p (character)
  (or (char= character #\Newline) (char= character #\Return)))

(defun yaml-indicator-p (character)
  (find character "-?:,[]{}#&*!|>'\"%@`" :test #'char=))

(defun yaml-whitespace-p (character)
  (reader-whitespace-p character))
