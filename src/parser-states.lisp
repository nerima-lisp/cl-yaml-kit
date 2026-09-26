;;;; src/parser-states.lisp
(in-package #:yaml-kit)

(defstruct (reader-parser (:constructor make-reader-parser%))
  state depth)
