;;;; src/emitter-scalars.lisp
(in-package #:yaml-kit)

(defun %plain-safe-p (value)
  (and (plusp (length value))
       (not (find (char value 0) "-?:,[]{}#&*!|>'\"%@`"))
       (not (find #\Newline value))
       (not (search " : " value))
       (not (search " #" value))
       (not (member value '("null" "Null" "NULL" "true" "True" "TRUE"
                            "false" "False" "FALSE" "~" ".nan" ".NaN"
                            ".NAN" ".inf" ".Inf" ".INF" "-.inf" "-.Inf"
                            "-.INF") :test #'string=))
       (not (ignore-errors (parse-integer value :junk-allowed nil)))))

(defun %yaml-double-quote (value)
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across value do
      (case char
        (#\" (write-string "\\\"" out))
        (#\\ (write-string "\\\\" out))
        (#\Newline (write-string "\\n" out))
        (#\Return (write-string "\\r" out))
        (#\Tab (write-string "\\t" out))
        (t (write-char char out))))
    (write-char #\" out)))

(defun %scalar-text (value style)
  (case style
    (:plain value)
    (:single-quoted (with-output-to-string (out)
                      (write-char #\' out)
                      (loop for char across value do
                        (write-char char out)
                        (when (char= char #\') (write-char #\' out)))
                      (write-char #\' out)))
    (:double-quoted (%yaml-double-quote value))
    (otherwise value)))
