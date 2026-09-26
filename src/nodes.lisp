;;;; src/nodes.lisp
(in-package #:yaml-kit)

(defmacro define-node (name slots &optional documentation)
  (declare (ignore documentation))
  (let ((constructor (intern (format nil "MAKE-~A" (string-upcase name)) *package*))
        (predicate (intern (format nil "~A-P" (string-upcase name)) *package*)))
    `(progn
       (defstruct (,name (:constructor ,constructor)
                            (:predicate ,predicate)
                            (:conc-name ,(intern (format nil "~A-" (string-upcase name))
                                                *package*)))
         (tag nil) (anchor nil) (style nil) (start-mark nil) (end-mark nil)
         ,@slots)
       ,name)))

(define-node scalar-node ((value "")) "A scalar node in a YAML representation graph.")
(define-node sequence-node ((items nil :type list)) "A sequence node in a YAML graph.")
(define-node mapping-node ((pairs nil :type list)) "A mapping node in a YAML graph.")

(defun node-tag (node) (slot-value node 'tag))
(defun node-anchor (node) (slot-value node 'anchor))
(defun node-style (node) (slot-value node 'style))
(defun node-start-mark (node) (slot-value node 'start-mark))
(defun node-end-mark (node) (slot-value node 'end-mark))
