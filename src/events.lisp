;;;; src/events.lisp
(in-package #:yaml-kit)

(defstruct (mark (:constructor make-mark (line column offset))
                 (:copier nil) (:predicate nil))
  (line 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (column 0 :type (integer 0 #.most-positive-fixnum) :read-only t)
  (offset 0 :type (integer 0 #.most-positive-fixnum) :read-only t))

(defstruct (event (:constructor make-event (&key start-mark end-mark))
                  (:copier nil) (:predicate nil))
  "Base class for YAML parsing and emitting events."
  (start-mark nil :type (or null mark))
  (end-mark nil :type (or null mark)))

(defmacro define-yaml-subtype (name base slots &optional documentation)
  (let ((constructor (intern (format nil "MAKE-~A" (string-upcase name)) *package*))
        (predicate (intern (format nil "~A-P" (string-upcase name)) *package*)))
    `(progn
       (defstruct (,name (:constructor ,constructor)
                            (:predicate ,predicate)
                            (:copier nil)
                            (:include ,base)
                            (:conc-name ,(intern (format nil "~A-" (string-upcase name))
                                                *package*)))
         ,@(when documentation (list documentation))
         ,@slots)
       ',name)))

(defmacro define-event (name slots &optional documentation)
  `(define-yaml-subtype ,name event ,slots ,documentation))

(define-event stream-start-event ())
(define-event stream-end-event ())
(define-event document-start-event
  ((explicit-p nil :type boolean) (version nil) (tag-directives nil :type list)))
(define-event document-end-event ((explicit-p nil :type boolean)))
(define-event sequence-start-event
  ((anchor nil :type (or null simple-string))
   (tag nil :type (or null simple-string))
   (implicit-p nil :type boolean) (style :block :type collection-style)))
(define-event sequence-end-event ())
(define-event mapping-start-event
  ((anchor nil :type (or null simple-string))
   (tag nil :type (or null simple-string))
   (implicit-p nil :type boolean) (style :block :type collection-style)))
(define-event mapping-end-event ())
(define-event scalar-event
  ((anchor nil :type (or null simple-string)) (tag nil :type (or null simple-string))
   (value "" :type simple-string) (plain-implicit-p nil :type boolean)
   (quoted-implicit-p nil :type boolean) (style :plain :type scalar-style)))
(define-event alias-event ((anchor nil :type (or null simple-string))))
