;;;; src/nodes.lisp
(in-package #:yaml-kit)

(defstruct (node (:constructor make-node (&key tag anchor style start-mark end-mark))
                 (:copier nil) (:predicate nil))
  "Base class for YAML representation graph nodes."
  (tag nil :type (or null simple-string))
  (anchor nil :type (or null simple-string))
  (style nil :type (or null scalar-style collection-style))
  (start-mark nil :type (or null mark))
  (end-mark nil :type (or null mark)))

(defmacro define-node (name slots &optional documentation)
  `(define-yaml-subtype ,name node ,slots ,documentation))

(define-node scalar-node ((value "" :type simple-string)))
(define-node sequence-node ((items nil :type list)))
(define-node mapping-node ((pairs nil :type list)))
