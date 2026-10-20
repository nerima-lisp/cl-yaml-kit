(in-package #:yaml-kit)

(defparameter *implicit-scalar-tags*
  (mapcar (lambda (suffix)
            (concatenate 'simple-string +yaml-tag-prefix+ suffix))
          '("int" "float" "bool" "null" "str")))

(defparameter *protected-scalar-values*
  '("true" "True" "TRUE" "false" "False" "FALSE"
    "null" "Null" "NULL" "~" "1.5" "0x1F"))
