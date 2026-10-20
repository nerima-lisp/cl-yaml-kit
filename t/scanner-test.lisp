;;;; t/scanner-test.lisp
(in-package #:cl-yaml-kit/test)

(defun scanner-source (text)
  (make-array (length text) :element-type 'character :initial-contents text))

(defun scanner-token-kinds (text)
  (let ((scanner (yaml-kit::make-scanner (scanner-source text)))
        (kinds nil))
    (loop for token = (yaml-kit::%scanner-next-token scanner)
          while token
          do (push (yaml-kit:token-kind token) kinds))
    (nreverse kinds)))

(describe "scanner character classes"

  (cl-weave:it-each
      ((digit-accepts-ascii yaml-kit::sc-digit-p "7" t)
       (digit-rejects-fullwidth yaml-kit::sc-digit-p "７" nil)
       (hex-accepts-ascii yaml-kit::sc-hex-p "F" t)
       (hex-rejects-fullwidth yaml-kit::sc-hex-p "Ｆ" nil)
       (alpha-accepts-ascii yaml-kit::sc-alpha-p "A" t)
       (alpha-accepts-ascii-digit yaml-kit::sc-alpha-p "7" t)
       (alpha-accepts-lowercase yaml-kit::sc-alpha-p "a" t)
       (alpha-rejects-unicode-digit yaml-kit::sc-alpha-p "あ" nil))
    "classifies scanner character input ~S"
    (name predicate text expected)
    (declare (ignore name))
    (let ((scanner (yaml-kit::make-scanner (scanner-source text))))
      (expect (funcall predicate scanner) :to-equal expected)))

  (cl-weave:it-each
      ((space-accepts-space yaml-kit::sc-space-p " " t)
       (space-rejects-tab yaml-kit::sc-space-p #.(string #\Tab) nil)
       (tab-accepts-tab yaml-kit::sc-tab-p #.(string #\Tab) t)
       (zero-accepts-end-of-input yaml-kit::sc-z-p "" t)
       (bom-accepts-bom yaml-kit::sc-bom-p "﻿" t)
       (printable-accepts-ascii yaml-kit::sc-printable-p "A" t)
       (printable-rejects-control yaml-kit::sc-printable-p #.(string (code-char 1)) nil))
    "classifies scanner space and printable input ~S"
    (name predicate text expected)
    (declare (ignore name))
    (let ((scanner (yaml-kit::make-scanner (scanner-source text))))
      (expect (funcall predicate scanner) :to-equal expected))))

(it "reads scanner characters and checks the current character"
  (declare (notinline yaml-kit::sc-char yaml-kit::sc-check))
  (let ((scanner (yaml-kit::make-scanner (scanner-source "A"))))
    (expect (yaml-kit::sc-char scanner) :to-equal #\A)
    (setf (yaml-kit::scanner-pos scanner) 1)
    (expect (yaml-kit::sc-char scanner) :to-equal #\Nul)
    (expect (yaml-kit::sc-check scanner #\A) :to-equal nil)))

(it "reports the initial scanner position and reads into a scan buffer"
  (let ((scanner (yaml-kit::make-scanner (scanner-source "x"))))
    (let ((position (funcall (symbol-function 'yaml-kit::sc-position) scanner)))
      (expect (list (yaml-kit::sc-position-line position)
                    (yaml-kit::sc-position-column position)
                    (yaml-kit::sc-position-offset position))
              :to-equal '(0 0 0)))
    (let ((buffer (yaml-kit::make-scan-buffer)))
      (expect (funcall (symbol-function 'yaml-kit::sc-read) scanner buffer) :to-be buffer)
      (expect (yaml-kit::scan-buffer-string buffer) :to-equal "x"))))

(it "advances the scanner position and column when skipping a character"
  (declare (notinline yaml-kit::sc-skip))
  (let ((scanner (yaml-kit::make-scanner (scanner-source "x"))))
    (yaml-kit::sc-skip scanner)
    (expect (list (yaml-kit::scanner-pos scanner)
                  (yaml-kit::scanner-column scanner))
            :to-equal '(1 1))))

(cl-weave:it-each
    ((crlf #.(format nil "~C~C" #\Return #\Newline))
     (line-feed #.(string #\Newline))
     (carriage-return #.(string #\Return))
     (ordinary-character "x"))
  "updates scanner line state after skipping ~S"
  (name text)
  (let ((scanner (yaml-kit::make-scanner (scanner-source text))))
    (declare (notinline yaml-kit::sc-skip-line))
    (funcall (symbol-function 'yaml-kit::sc-skip-line) scanner)
    (expect (list (yaml-kit::scanner-line scanner)
                  (yaml-kit::scanner-column scanner))
            :to-equal '(1 0))))

(it "reads line terminators into the scan buffer"
  (let ((scanner (yaml-kit::make-scanner (scanner-source "x")))
        (buffer (yaml-kit::make-scan-buffer)))
    (expect (funcall (symbol-function 'yaml-kit::sc-read-line) scanner buffer) :to-be buffer)
    (expect (yaml-kit::scan-buffer-string buffer) :to-equal (format nil "~%"))))

(cl-weave:it-each
    ((zero 0 0)
     (upper-hex 1 10)
     (lower-hex 2 15))
  "converts scanner hexadecimal digit ~S"
  (name index expected)
  (declare (ignore name))
  (declare (notinline yaml-kit::sc-hex-value))
  (let ((scanner (yaml-kit::make-scanner (scanner-source "0Af"))))
    (expect (yaml-kit::sc-hex-value scanner index) :to-equal expected)))

(it "signals a YAML parse error for an invalid hexadecimal digit"
  (let ((scanner (yaml-kit::make-scanner (scanner-source "g"))))
    (expect (nth-value 1 (ignore-errors (yaml-kit::sc-hex-value scanner 0)))
            :to-be-type-of 'yaml-kit:yaml-parse-error)))

(it "does not advance past the end of scanner input"
  (let ((scanner (yaml-kit::make-scanner (scanner-source ""))))
    (yaml-kit::sc-skip scanner)
    (expect (list (yaml-kit::scanner-pos scanner)
                  (yaml-kit::scanner-column scanner))
            :to-equal '(0 0))))

(describe "scanner token kinds"
  (cl-weave:it-each
      ((plain-mapping-and-simple-key "a: 1"
         (:stream-start :block-mapping-start :key :scalar :value :scalar
          :block-end :stream-end))
       (flow-mapping-and-simple-key "{a: b}"
         (:stream-start :flow-mapping-start :key :scalar :value :scalar
          :flow-mapping-end :stream-end))
       (explicit-key "? a"
         (:stream-start :block-mapping-start :key :scalar :block-end :stream-end))
       ;; A line break inside a plain scalar folds into the same scalar token.
       (plain-scalar-folds-a-line-break #.(format nil "a~%b")
         (:stream-start :scalar :stream-end))
       (simple-key-at-the-1024-character-limit
         #.(concatenate 'string (make-string 1024 :initial-element #\a) ": b")
         (:stream-start :block-mapping-start :key :scalar :value :scalar
          :block-end :stream-end))
       (nested-block-mapping-unrolls-indentation #.(format nil "a:~%  b: 1~%c: 2")
         (:stream-start :block-mapping-start :key :scalar :value
          :block-mapping-start :key :scalar :value :scalar :block-end
          :key :scalar :value :scalar :block-end :stream-end))
       ;; An indentless sequence shares the indentation of the mapping that owns it,
       ;; so the mapping contributes the only BLOCK-END.
       (indentless-sequence #.(format nil "a:~%- b")
         (:stream-start :block-mapping-start :key :scalar :value :block-entry
          :scalar :block-end :stream-end))
       ;; A compact mapping rolls its own indentation, so a nested start precedes its key.
       (nested-block-sequence-and-mapping #.(format nil "a:~%  - b~%  - c: d")
         (:stream-start :block-mapping-start :key :scalar :value
          :block-sequence-start :block-entry :scalar :block-entry
          :block-mapping-start :key :scalar :value :scalar :block-end
          :block-end :block-end :stream-end))
       (flow-scalar-tags "!!str !e!foo" (:stream-start :tag :tag :stream-end))
       (block-plain-scalar-begins-with-comma ",word"
         (:stream-start :scalar :stream-end))
       (bare-comma-is-plain-scalar "," (:stream-start :scalar :stream-end)))
    "scans ~S into the expected token kinds"
    (name text expected)
    (declare (ignore name))
    (expect (scanner-token-kinds text) :to-equal expected)))

(it "peeking a token does not consume it and preserves its end span"
  (let* ((source (scanner-source "a"))
         (scanner (yaml-kit::make-scanner source)))
    (yaml-kit::%scanner-next-token scanner)
    (let* ((peeked (yaml-kit::%scanner-peek-token scanner))
           (next (yaml-kit::%scanner-next-token scanner))
           (span (yaml-kit:token-end-span peeked)))
      (expect (eq peeked next) :to-be-truthy)
      (expect (list (cl-parser-kit:span-start-line span)
                    (cl-parser-kit:span-start-column span)
                    (cl-parser-kit:span-start span)
                    (cl-parser-kit:span-end span))
              :to-equal '(1 2 1 1))
      (expect (eq (cl-parser-kit:span-source span) source)
              :to-be-truthy))))
