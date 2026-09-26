;;;; src/tokens.lisp
(in-package #:yaml-kit)

(defstruct (token (:constructor make-token
                    (kind start-mark end-mark
                     &key value handle suffix style major minor)))
  (kind :stream-start :type keyword :read-only t)
  (start-mark nil :type mark :read-only t)
  (end-mark nil :type mark :read-only t)
  (value nil :type (or null simple-string) :read-only t)
  (handle nil :type (or null simple-string) :read-only t)
  (suffix nil :type (or null simple-string) :read-only t)
  (style nil :type (or null scalar-style) :read-only t)
  (major nil :type (or null fixnum) :read-only t)
  (minor nil :type (or null fixnum) :read-only t))
