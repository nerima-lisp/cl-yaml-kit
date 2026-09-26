;;;; src/reader-macros.lisp
(in-package #:yaml-kit)

(defmacro with-reader-speed (&body body)
  `(locally (declare (optimize (speed 3) (safety 1) (debug 0))) ,@body))

(defmacro reader-char (string index)
  `(char ,string ,index))

(defmacro reader-whitespace-p (character)
  `(or (char= ,character #\Space) (char= ,character #\Tab)))
