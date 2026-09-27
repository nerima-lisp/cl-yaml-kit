;;;; src/tokens.lisp
(in-package #:yaml-kit)

(defstruct (token (:constructor %make-token
                    (kind start-mark end-mark
                     &key value handle suffix style major minor))
                (:copier nil) (:predicate nil))
  (kind :stream-start :type keyword)
  (start-mark nil :type mark)
  (end-mark nil :type mark)
  (value nil :type (or null simple-string))
  (handle nil :type (or null simple-string))
  (suffix nil :type (or null simple-string))
  (style nil :type (or null scalar-style))
  (major nil :type (or null fixnum))
  (minor nil :type (or null fixnum)))

(defun token-ends-json-like-node-p (token)
  "True when TOKEN closes a node that c-flow-json-value allows as a flow key.
YAML 1.2.2 spells that set as a quoted scalar or a completed flow collection;
both surround the node with indicators, so a following \":\" needs no separation."
  (and (or (and (eq (token-kind token) :scalar)
                 (member (token-style token) '(:single-quoted :double-quoted)))
           (member (token-kind token) '(:flow-sequence-end :flow-mapping-end)))
       t))

(defun make-token (kind start-mark end-mark &key value handle suffix style major minor)
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
