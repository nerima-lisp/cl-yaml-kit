;;;; src/scanner-structure.lisp
(in-package #:yaml-kit)

(defun reader-count-indent (line)
  (loop for i fixnum from 0 below (length line)
        while (char= (char line i) #\Space) finally (return i)))
