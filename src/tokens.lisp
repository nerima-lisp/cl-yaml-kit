;;;; src/tokens.lisp
(in-package #:yaml-kit)

(defstruct (token (:constructor %make-token
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

(defun make-token (kind start-mark end-mark &key value handle suffix style major minor)
  "Construct a token after validating every optional payload field."
  (check-type kind keyword)
  (check-type start-mark mark)
  (check-type end-mark mark)
  (check-type value (or null simple-string))
  (check-type handle (or null simple-string))
  (check-type suffix (or null simple-string))
  (check-type style (or null scalar-style))
  (check-type major (or null fixnum))
  (check-type minor (or null fixnum))
  (%make-token kind start-mark end-mark
               :value value :handle handle :suffix suffix :style style
               :major major :minor minor))
