;;;; src/scanner-state.lisp
(in-package #:yaml-kit)

(defstruct (reader-state (:constructor make-reader-state%))
  (text "" :type simple-string)
  (position 0 :type fixnum)
  (line 1 :type fixnum)
  (column 1 :type fixnum)
  (max-input-length 104857600 :type fixnum)
  (max-depth 256 :type fixnum)
  (max-scalar-length 16777216 :type fixnum)
  (handler nil))
