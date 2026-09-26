;;;; src/char-classes.lisp
(in-package #:yaml-kit)

(defparameter +yaml-indicator-table+ "-?:,[]{}#&*!|>'\"%@`")
(defparameter +yaml-flow-indicator-table+ "[],{}")

(defun %make-character-class-table (characters)
  (let ((table (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
    (loop for character across characters
          do (setf (aref table (char-code character)) 1))
    table))

(defparameter +yaml-indicator-bits+ (%make-character-class-table +yaml-indicator-table+))
(defparameter +yaml-flow-indicator-bits+ (%make-character-class-table +yaml-flow-indicator-table+))

(declaim (inline yaml-indicator-p yaml-flow-indicator-p yaml-line-break-p yaml-whitespace-p))
(defun yaml-indicator-p (character)
  (and (< (char-code character) 128) (= 1 (aref +yaml-indicator-bits+ (char-code character)))))
(defun yaml-flow-indicator-p (character)
  (and (< (char-code character) 128) (= 1 (aref +yaml-flow-indicator-bits+ (char-code character)))))
(defun yaml-line-break-p (character)
  (or (char= character #\Newline) (char= character #\Return)))
(defun yaml-whitespace-p (character)
  (or (char= character #\Space) (char= character #\Tab)))
