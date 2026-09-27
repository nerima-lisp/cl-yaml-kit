;;;; src/data.lisp
(in-package #:yaml-kit)

(defconstant +yaml-null+ (intern "%YAML-NULL" '#:yaml-kit))
(defconstant +yaml-false+ (intern "%YAML-FALSE" '#:yaml-kit))

(declaim (inline yaml-null-p yaml-false-p))
(defun yaml-null-p (value) (eq value +yaml-null+))
(defun yaml-false-p (value) (eq value +yaml-false+))

(defstruct (yaml-mapping (:constructor %make-yaml-mapping (entries))
                         (:copier nil) (:conc-name %yaml-mapping-))
  (entries nil :type list :read-only t))

(defun make-yaml-mapping (&optional (entries nil))
  (%make-yaml-mapping (copy-list entries)))

(defun yaml-mapping-entries (mapping)
  (%yaml-mapping-entries mapping))

(deftype scalar-style () '(member :plain :single-quoted :double-quoted :literal :folded))
(deftype collection-style () '(member :block :flow))
(deftype chomping-indicator () '(member :clip :strip :keep))
(deftype indentation-indicator () '(or null (integer 1 9)))
