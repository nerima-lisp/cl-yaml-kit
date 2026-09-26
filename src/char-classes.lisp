;;;; src/char-classes.lisp
(in-package #:yaml-kit)

(defparameter +yaml-indicator-table+ "-?:,[]{}#&*!|>'\"%@`")
(defparameter +yaml-flow-indicator-table+ "[],{}")

(define-yaml-production indicator "-?:,[]{}#&*!|>'\"%@`")
(define-yaml-production flow-indicator "[],{}")
(define-yaml-production line-break '(#\Newline #\Return))
(define-yaml-production whitespace '(#\Space #\Tab))
