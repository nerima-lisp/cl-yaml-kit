;;;; t/scanner-test.lisp
(in-package #:cl-yaml-kit/test)
(defparameter *invalid-scanner-state-slot-value* -1)

(defun scanner-source (text)
  (make-array (length text) :element-type 'character :initial-contents text))

(defun scanner-token-kinds (text)
  (let ((scanner (yaml-kit::make-scanner (scanner-source text)))
        (kinds nil))
    (loop for token = (yaml-kit::scanner-next-token scanner)
          while token
          do (push (yaml-kit:token-kind token) kinds))
    (nreverse kinds)))

(defmacro scanner-cases (table)
  `(dolist (case ,table)
     (it (first case)
       (expect (scanner-token-kinds (second case)) :to-equal (third case)))))

(describe "scanner character classes"
  (dolist (case '(("digit accepts ASCII" yaml-kit::sc-digit-p "7" t)
                  ("digit rejects fullwidth" yaml-kit::sc-digit-p "７" nil)
                  ("hex accepts ASCII" yaml-kit::sc-hex-p "F" t)
                  ("hex rejects fullwidth" yaml-kit::sc-hex-p "Ｆ" nil)
                  ("alpha accepts ASCII" yaml-kit::sc-alpha-p "A" t)
                  ("alpha accepts ASCII digit" yaml-kit::sc-alpha-p "7" t)
                  ("alpha accepts lowercase" yaml-kit::sc-alpha-p "a" t)
                  ("alpha rejects Unicode digit" yaml-kit::sc-alpha-p "あ" nil)))
    (destructuring-bind (name predicate text expected) case
      (it name
        (let ((scanner (yaml-kit::make-scanner (scanner-source text))))
          (expect (funcall predicate scanner) :to-equal expected))))))
  (dolist (case '(("space accepts space" yaml-kit::sc-space-p " " t)
                  ("space rejects tab" yaml-kit::sc-space-p #.(string #\Tab) nil)
                  ("tab accepts tab" yaml-kit::sc-tab-p #.(string #\Tab) t)
                  ("zero accepts end of input" yaml-kit::sc-z-p "" t)
                  ("BOM accepts BOM" yaml-kit::sc-bom-p "﻿" t)
                  ("printable accepts ASCII" yaml-kit::sc-printable-p "A" t)
                  ("printable rejects control" yaml-kit::sc-printable-p #.(string (code-char 1)) nil)))
    (destructuring-bind (name predicate text expected) case
      (it name
        (let ((scanner (yaml-kit::make-scanner (scanner-source text))))
          (expect (funcall predicate scanner) :to-equal expected)))))

(it "exercises scanner state primitives at boundaries"
  (declare (notinline yaml-kit::sc-char yaml-kit::sc-check yaml-kit::sc-skip
                      yaml-kit::sc-skip-line yaml-kit::sc-hex-value))
  (let ((scanner (yaml-kit::make-scanner (scanner-source "A"))))
    (expect (yaml-kit::sc-char scanner) :to-equal #\A)
    (setf (yaml-kit::scanner-pos scanner) 1)
    (expect (yaml-kit::sc-char scanner) :to-equal #\Nul)
    (expect (yaml-kit::sc-check scanner #\A) :to-equal nil))
  (let ((scanner (yaml-kit::make-scanner (scanner-source "x"))))
    (let ((position (funcall (symbol-function 'yaml-kit::sc-position) scanner)))
      (expect (list (yaml-kit::sc-position-line position)
                    (yaml-kit::sc-position-column position)
                    (yaml-kit::sc-position-offset position))
              :to-equal '(0 0 0)))
    (let ((buffer (yaml-kit::make-scan-buffer)))
      (expect (funcall (symbol-function 'yaml-kit::sc-read) scanner buffer) :to-be buffer)
      (expect (yaml-kit::scan-buffer-string buffer) :to-equal "x"))
    (setf (yaml-kit::scanner-pos scanner) 0
          (yaml-kit::scanner-column scanner) 0)
    (yaml-kit::sc-skip scanner)
    (expect (yaml-kit::scanner-pos scanner) :to-equal 1)
    (expect (yaml-kit::scanner-column scanner) :to-equal 1))
  (dolist (text (list (format nil "~C~C" #\Return #\Newline) (string #\Newline) (string #\Return) "x"))
    (let ((scanner (yaml-kit::make-scanner (scanner-source text))))
      (funcall (symbol-function 'yaml-kit::sc-skip-line) scanner)
      (expect (yaml-kit::scanner-line scanner) :to-equal 1)
      (expect (yaml-kit::scanner-column scanner) :to-equal 0)))
  (let ((scanner (yaml-kit::make-scanner (scanner-source "x")))
        (buffer (yaml-kit::make-scan-buffer)))
    (expect (funcall (symbol-function 'yaml-kit::sc-read-line) scanner buffer) :to-be buffer)
    (expect (yaml-kit::scan-buffer-string buffer) :to-equal (format nil "~%")))
  (let ((scanner (yaml-kit::make-scanner (scanner-source "0Af"))))
    (expect (yaml-kit::sc-hex-value scanner 0) :to-equal 0)
    (expect (yaml-kit::sc-hex-value scanner 1) :to-equal 10)
    (expect (yaml-kit::sc-hex-value scanner 2) :to-equal 15))
  (let ((scanner (yaml-kit::make-scanner (scanner-source "g"))))
    (expect (handler-case (yaml-kit::sc-hex-value scanner 0)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)))
(it "does not advance past end of scanner input"
  (let ((scanner (yaml-kit::make-scanner (scanner-source ""))))
    (yaml-kit::sc-skip scanner)
    (expect (list (yaml-kit::scanner-pos scanner)
                  (yaml-kit::scanner-column scanner))
            :to-equal (quote (0 0)))))

(defparameter *scanner-token-cases*
  (list
   (list "plain mapping and implicit simple key"
         "a: 1"
         '(:stream-start :block-mapping-start :key :scalar :value :scalar
           :block-end :stream-end))
   (list "flow mapping and implicit simple key"
         "{a: b}"
         '(:stream-start :flow-mapping-start :key :scalar :value :scalar
           :flow-mapping-end :stream-end))
   (list "explicit key"
         "? a"
         '(:stream-start :block-mapping-start :key :scalar :block-end
           :stream-end))
   ;; A line break folds into the plain scalar, so nothing on the next line can
   ;; close a simple key opened on the previous one.
   (list "plain scalar folds a line break"
         (format nil "a~%b")
         '(:stream-start :scalar :stream-end))
   (list "simple key at the 1024 character limit"
         (concatenate 'string (make-string 1024 :initial-element #\a) ": b")
         '(:stream-start :block-mapping-start :key :scalar :value :scalar
           :block-end :stream-end))
   (list "nested block mapping unrolls its indentation"
         (format nil "a:~%  b: 1~%c: 2")
         '(:stream-start :block-mapping-start :key :scalar :value
           :block-mapping-start :key :scalar :value :scalar :block-end
           :key :scalar :value :scalar :block-end :stream-end))
   ;; An indentless sequence shares the indentation of the mapping that owns it,
   ;; so the mapping contributes the only BLOCK-END.
   (list "indentless sequence"
         (format nil "a:~%- b")
         '(:stream-start :block-mapping-start :key :scalar :value :block-entry
           :scalar :block-end :stream-end))
   ;; "- c: d" is a compact mapping, and a compact mapping rolls its own
   ;; indentation, so a nested BLOCK-MAPPING-START precedes its key.
   (list "nested block sequence and mapping"
         (format nil "a:~%  - b~%  - c: d")
         '(:stream-start :block-mapping-start :key :scalar :value
           :block-sequence-start :block-entry :scalar :block-entry
           :block-mapping-start :key :scalar :value :scalar :block-end
           :block-end :block-end :stream-end))
   (list "flow scalar tags"
         "!!str !e!foo"
         '(:stream-start :tag :tag :stream-end))))

(defparameter *scanner-block-indicator-cases*
  (list
   (list "block plain scalar begins with comma" ",word"
         '(:stream-start :scalar :stream-end))))

(describe "scanner token kinds"
  (scanner-cases *scanner-token-cases*)
  (scanner-cases *scanner-block-indicator-cases*))

(it "peeking a token does not consume it and preserves its end span"
  (let* ((source (scanner-source "a"))
         (scanner (yaml-kit::make-scanner source)))
    (yaml-kit::scanner-next-token scanner)
    (let* ((peeked (yaml-kit::scanner-peek-token scanner))
           (next (yaml-kit::scanner-next-token scanner))
           (span (yaml-kit:token-end-span peeked)))
      (expect (eq peeked next) :to-be-truthy)
      (expect (list (cl-parser-kit:span-start-line span)
                    (cl-parser-kit:span-start-column span)
                    (cl-parser-kit:span-start span)
                    (cl-parser-kit:span-end span))
              :to-equal (quote (1 2 1 1)))
      (expect (eq (cl-parser-kit:span-source span) source)
              :to-be-truthy))))

(it "reuses a recycled token for the next scanner token"
  (let* ((scanner (yaml-kit::make-scanner (scanner-source "payload")))
         (seeded-span (test-point-span 9 8 7))
         (recycled (yaml-kit:make-token :tag seeded-span seeded-span
                                        :handle "!e!" :suffix "old")))
    (setf (yaml-kit::scanner-token-pool scanner) (list recycled))
    (let ((stream-start (yaml-kit::scanner-next-token scanner)))
      (expect (list (eq stream-start recycled)
                    (yaml-kit:token-kind stream-start)
                    (yaml-kit:token-value stream-start)
                    (yaml-kit:token-handle stream-start)
                    (yaml-kit:token-suffix stream-start)
                    (yaml-kit:token-style stream-start)
                    (yaml-kit:token-major stream-start)
                    (yaml-kit:token-minor stream-start)
                    (list (cl-parser-kit:span-start-line (yaml-kit:token-start-span stream-start))
                          (cl-parser-kit:span-start-column (yaml-kit:token-start-span stream-start))
                          (cl-parser-kit:span-start (yaml-kit:token-start-span stream-start)))
                    (list (cl-parser-kit:span-start-line (yaml-kit:token-end-span stream-start))
                          (cl-parser-kit:span-start-column (yaml-kit:token-end-span stream-start))
                          (cl-parser-kit:span-start (yaml-kit:token-end-span stream-start))))
              :to-equal (quote (t :stream-start nil nil nil nil nil nil
                                  (1 1 0) (1 1 0))))
      (yaml-kit::recycle-scanner-token scanner)
      (expect (eq (first (yaml-kit::scanner-token-pool scanner)) stream-start)
              :to-be-truthy)
      (let ((scalar (yaml-kit::scanner-next-token scanner)))
        (expect (list (eq scalar recycled)
                      (yaml-kit:token-kind scalar)
                      (yaml-kit:token-value scalar)
                      (yaml-kit:token-handle scalar)
                      (yaml-kit:token-suffix scalar)
                      (yaml-kit:token-style scalar)
                      (yaml-kit:token-major scalar)
                      (yaml-kit:token-minor scalar)
                      (list (cl-parser-kit:span-start-line (yaml-kit:token-start-span scalar))
                            (cl-parser-kit:span-start-column (yaml-kit:token-start-span scalar))
                            (cl-parser-kit:span-start (yaml-kit:token-start-span scalar)))
                      (list (cl-parser-kit:span-start-line (yaml-kit:token-end-span scalar))
                            (cl-parser-kit:span-start-column (yaml-kit:token-end-span scalar))
                            (cl-parser-kit:span-start (yaml-kit:token-end-span scalar)))
                      (yaml-kit::scanner-previous-token-kind scanner))
                :to-equal (quote (t :scalar "payload" nil nil :plain nil nil
                                  (1 1 0) (1 8 7) :scalar)))))))
(it "rejects invalid scanner state slot values"
  (let ((scanner (yaml-kit::make-scanner (scanner-source ""))))
    (expect (every (lambda (thunk)
                     (handler-case (funcall thunk)
                       (type-error () t)))
                   (list (lambda ()
                           (setf (yaml-kit::scanner-recyclable-token scanner)
                                 *invalid-scanner-state-slot-value*))
                         (lambda ()
                           (setf (yaml-kit::scanner-previous-token-kind scanner)
                                 *invalid-scanner-state-slot-value*))))
            :to-be-truthy)))

(it "preserves a long token sequence across queue compaction"
  (let* ((values (loop for index below 40 collect (format nil "item-~D" index)))
         (scanner (yaml-kit::make-scanner
                   (scanner-source (format nil "[~{~A~^,~}]" values))))
         (kinds nil)
         (scalars nil)
         (previous-head 0)
         (compacted-p nil))
    (loop for token = (yaml-kit::scanner-next-token scanner)
          while token
          do (let ((head (yaml-kit::scanner-tokens-head scanner)))
               (when (< head previous-head)
                 (setf compacted-p t))
               (setf previous-head head))
             (push (yaml-kit:token-kind token) kinds)
             (when (eq (yaml-kit:token-kind token) :scalar)
               (push (yaml-kit:token-value token) scalars)))
    (expect (list (> (length kinds) 64) compacted-p)
            :to-equal (quote (t t)))
    (expect (nreverse kinds)
            :to-equal
            (append (quote (:stream-start :flow-sequence-start))
                    (loop for index below (length values)
                          append (if (< index (1- (length values)))
                                     (quote (:scalar :flow-entry))
                                     (quote (:scalar))))
                    (quote (:flow-sequence-end :stream-end))))
    (expect (nreverse scalars) :to-equal values)))
