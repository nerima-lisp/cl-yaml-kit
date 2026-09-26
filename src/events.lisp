;;;; src/events.lisp
(in-package #:yaml-kit)

(defstruct (mark (:constructor make-mark (line column offset)))
  (line 0 :type unsigned-byte) (column 0 :type unsigned-byte)
  (offset 0 :type unsigned-byte))

(defmacro define-event (name slots &optional documentation)
  (declare (ignore documentation))
  (let ((constructor (intern (format nil "MAKE-~A" (string-upcase name)) *package*))
        (predicate (intern (format nil "~A-P" (string-upcase name)) *package*)))
    `(progn
       (defstruct (,name (:constructor ,constructor)
                            (:predicate ,predicate)
                            (:conc-name ,(intern (format nil "~A-" (string-upcase name))
                                                *package*)))
         ,@(unless (eq name 'event)
             '((start-mark nil :type mark) (end-mark nil :type mark)))
         ,@slots)
       ,name)))

(define-event stream-start-event () "Start of a YAML stream.")
(define-event stream-end-event () "End of a YAML stream.")
(define-event document-start-event
  ((explicit-p nil :type boolean) (version nil) (tag-directives nil :type list)))
(define-event document-end-event ((explicit-p nil :type boolean)))
(define-event sequence-start-event
  ((anchor nil) (tag nil) (implicit-p nil :type boolean) (style :block)))
(define-event sequence-end-event ())
(define-event mapping-start-event
  ((anchor nil) (tag nil) (implicit-p nil :type boolean) (style :block)))
(define-event mapping-end-event ())
(define-event scalar-event
  ((anchor nil) (tag nil) (value "") (plain-implicit-p nil :type boolean)
   (quoted-implicit-p nil :type boolean) (style :plain)))
(define-event alias-event ((anchor nil)))

(defun event-start-mark (event) (slot-value event 'start-mark))
(defun event-end-mark (event) (slot-value event 'end-mark))
