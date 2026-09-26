;;;; src/nodes.lisp
(in-package #:yaml-kit)

(defstruct (node (:constructor make-node (&key tag anchor style start-mark end-mark)))
  "Base class for YAML representation graph nodes."
  (tag nil :type (or null simple-string))
  (anchor nil :type (or null simple-string))
  (style nil :type (or null scalar-style collection-style))
  (start-mark nil :type (or null mark))
  (end-mark nil :type (or null mark)))

(defmacro define-node (name slots &optional documentation)
  (let ((constructor (intern (format nil "MAKE-~A" (string-upcase name)) *package*))
        (predicate (intern (format nil "~A-P" (string-upcase name)) *package*)))
    `(progn
       (defstruct (,name (:constructor ,constructor)
                            (:predicate ,predicate)
                            (:include node)
                            (:conc-name ,(intern (format nil "~A-" (string-upcase name))
                                                *package*)))
         ,@(when documentation (list documentation))
         ,@slots)
       ',name)))

(define-node scalar-node ((value "" :type simple-string)) "A scalar node in a YAML representation graph.")
(define-node sequence-node ((items nil :type list)) "A sequence node in a YAML graph.")
(define-node mapping-node ((pairs nil :type list)) "A mapping node in a YAML graph.")
