;;;; src/scanner-data.lisp
(in-package #:yaml-kit)

(defconstant +scanner-document-start-indicator+ #\-)
(defconstant +scanner-document-end-indicator+ #\.)
(defconstant +scanner-key-indicator+ #\?)
(defconstant +scanner-value-indicator+ #\:)

(defconstant +scanner-flow-block-entry-terminators+
  (if (boundp '+scanner-flow-block-entry-terminators+)
      (symbol-value '+scanner-flow-block-entry-terminators+)
      '(#\, #\] #\})))

(defconstant +scanner-flow-value-terminators+
  (if (boundp '+scanner-flow-value-terminators+)
      (symbol-value '+scanner-flow-value-terminators+)
      '(#\, #\? #\[ #\] #\{ #\})))

(defconstant +scanner-flow-plain-terminators+
  (if (boundp '+scanner-flow-plain-terminators+)
      (symbol-value '+scanner-flow-plain-terminators+)
      '(#\, #\[ #\] #\{ #\})))

(defconstant +scanner-key-value-indicators+
  (if (boundp '+scanner-key-value-indicators+)
      (symbol-value '+scanner-key-value-indicators+)
      '(#\? #\:)))

(defparameter *scanner-uri-reserved-characters* ";/?:@&=+$.!~*'()%_")
(defparameter *scanner-uri-flow-characters* ",[]")

(defparameter *flow-escape-table*
  ;; The escape table accepts \/ and a backslash followed by a literal tab.
  (let ((table (make-array 128 :initial-element nil)))
    (dolist (entry '((#\0 . 0) (#\a . 7) (#\b . 8) (#\t . 9) (#\Tab . 9)
                     (#\n . 10) (#\v . 11) (#\f . 12) (#\r . 13) (#\e . 27)
                     (#\Space . 32) (#\" . 34) (#\/ . 47) (#\\ . 92)
                     (#\N . #x85) (#\_ . #xa0) (#\L . #x2028) (#\P . #x2029)))
      (setf (aref table (char-code (car entry))) (cdr entry)))
    table))
