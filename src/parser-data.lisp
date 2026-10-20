(in-package #:yaml-kit)

(defparameter *default-tag-directives*
  (list (cons "!" "!") (cons "!!" +yaml-tag-prefix+)))
