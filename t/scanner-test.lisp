;;;; t/scanner-test.lisp
(in-package #:cl-yaml-kit/test)

(defun scanner-source (text)
  (make-array (length text) :element-type 'character :initial-contents text))

(defun scanner-token-kinds (text)
  (let ((scanner (yaml-kit:make-scanner (scanner-source text)))
        (kinds nil))
    (loop for token = (yaml-kit:scanner-next-token scanner)
          while token
          do (push (yaml-kit:token-kind token) kinds))
    (nreverse kinds)))

(defmacro scanner-cases (table)
  `(dolist (case ,table)
     (it (first case)
       (expect (scanner-token-kinds (second case)) :to-equal (third case)))))

(describe "scanner character classes"
  (it "expands character productions and defines their predicates"
    (let ((expansion
            (macroexpand-1
             '(yaml-kit::%define-character-production probe-character "AZ"))))
      (expect (first expansion) :to-equal 'progn))
    (eval '(yaml-kit::%define-character-production probe-character-eval "AZ"))
    (let ((predicate (find-symbol "YAML-PROBE-CHARACTER-EVAL-P" *package*)))
      (expect (funcall (symbol-function predicate) #\A) :to-be-truthy)
      (expect (funcall (symbol-function predicate) #\B) :to-equal nil)
      (expect (funcall (symbol-function predicate) 1) :to-equal nil))
    (eval '(yaml-kit::%define-character-production probe-character-list
             '(#\A #\Z)))
    (let ((predicate (find-symbol "YAML-PROBE-CHARACTER-LIST-P" *package*)))
      (expect (funcall (symbol-function predicate) #\Z) :to-be-truthy)
      (expect (funcall (symbol-function predicate) #\B) :to-equal nil)))
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
        (let ((scanner (yaml-kit:make-scanner (scanner-source text))))
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
        (let ((scanner (yaml-kit:make-scanner (scanner-source text))))
          (expect (funcall predicate scanner) :to-equal expected)))))

(it "exercises scanner state primitives at boundaries"
  (declare (notinline yaml-kit::sc-char yaml-kit::sc-check yaml-kit::sc-skip
                      yaml-kit::sc-skip-line yaml-kit::sc-hex-value))
  (let ((scanner (yaml-kit:make-scanner (scanner-source "A"))))
    (expect (yaml-kit::sc-char scanner) :to-equal #\A)
    (setf (yaml-kit::scanner-pos scanner) 1)
    (expect (yaml-kit::sc-char scanner) :to-equal #\Nul)
    (expect (yaml-kit::sc-check scanner #\A) :to-equal nil))
  (let ((scanner (yaml-kit:make-scanner (scanner-source "x"))))
    (let ((mark (funcall (symbol-function 'yaml-kit::sc-mark) scanner)))
      (expect (list (yaml-kit:mark-line mark)
                    (yaml-kit:mark-column mark)
                    (yaml-kit:mark-offset mark))
              :to-equal '(0 0 0)))
    (let ((buffer (yaml-kit::make-scan-buffer)))
      (expect (funcall (symbol-function 'yaml-kit::sc-read) scanner buffer) :to-be buffer)
      (expect (yaml-kit::scan-buffer-string buffer) :to-equal "x"))
    (setf (yaml-kit::scanner-pos scanner) 0
          (yaml-kit::scanner-column scanner) 0)
    (yaml-kit::sc-skip scanner)
    (expect (yaml-kit::scanner-pos scanner) :to-equal 1)
    (expect (yaml-kit::scanner-column scanner) :to-equal 1))
  (dolist (text '("\r\n" "\n" "\r" "x"))
    (let ((scanner (yaml-kit:make-scanner (scanner-source text))))
      (funcall (symbol-function 'yaml-kit::sc-skip-line) scanner)
      (expect (yaml-kit::scanner-line scanner) :to-equal 1)
      (expect (yaml-kit::scanner-column scanner) :to-equal 0)))
  (let ((scanner (yaml-kit:make-scanner (scanner-source "x")))
        (buffer (yaml-kit::make-scan-buffer)))
    (expect (funcall (symbol-function 'yaml-kit::sc-read-line) scanner buffer) :to-be buffer)
    (expect (yaml-kit::scan-buffer-string buffer) :to-equal (format nil "~%")))
  (let ((scanner (yaml-kit:make-scanner (scanner-source "0Af"))))
    (expect (yaml-kit::sc-hex-value scanner 0) :to-equal 0)
    (expect (yaml-kit::sc-hex-value scanner 1) :to-equal 10)
    (expect (yaml-kit::sc-hex-value scanner 2) :to-equal 15))
  (let ((scanner (yaml-kit:make-scanner (scanner-source "g"))))
    (expect (handler-case (yaml-kit::sc-hex-value scanner 0)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)))

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

(it "peeking a token does not consume it and preserves its end mark"
  (let* ((scanner (yaml-kit:make-scanner (scanner-source "a")))
         (peeked (yaml-kit:scanner-peek-token scanner))
         (next (yaml-kit:scanner-next-token scanner)))
    (expect (eq peeked next) :to-be-truthy)
    (expect (list (yaml-kit:mark-line (yaml-kit:token-end-mark peeked))
                  (yaml-kit:mark-column (yaml-kit:token-end-mark peeked))
                  (yaml-kit:mark-offset (yaml-kit:token-end-mark peeked)))
            :to-equal '(0 0 0))))
