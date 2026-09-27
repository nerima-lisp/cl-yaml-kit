;;;; t/reader-test.lisp
(in-package #:cl-yaml-kit/test)

(defun block-scalar-input (format-control &rest arguments)
  (let ((text (apply #'format nil format-control arguments)))
    (make-array (length text) :element-type 'character :initial-contents text)))

(describe "block scalar reader"
  (it "EMPTY-LITERAL/EMPTY-FOLDED: keeps empty scalars empty"
    (let ((value (yaml-kit:parse (block-scalar-input "literal: |+~%"))))
      (expect (gethash "literal" value) :to-equal ""))
    (let ((value (yaml-kit:parse (block-scalar-input "folded: >-~%"))))
      (expect (gethash "folded" value) :to-equal "")))
  (it "FOLD-CHOMP: applies folding and chomping independently"
    (let ((value (yaml-kit:parse
                  (block-scalar-input "folded: >~%  first~%  second~%~%  third~%"))))
      (expect (gethash "folded" value)
              :to-equal (format nil "first second~%third~%")))
    (let ((value (yaml-kit:parse
                  (block-scalar-input "stripped: |-~%  value~%~%kept: |+~%  value~%~%"))))
      (expect (gethash "stripped" value) :to-equal "value")
      (expect (gethash "kept" value) :to-equal (format nil "value~%~%"))))
  (it "EXPLICIT-INDENT: uses the indentation indicator"
    (let ((value (yaml-kit:parse (block-scalar-input "value: |2~%    indented~%"))))
      (expect (gethash "value" value) :to-equal (format nil "  indented~%"))))
  (it "TAB-INDENT: rejects a tab used for block indentation"
    (expect (handler-case
                (progn (yaml-kit:parse (block-scalar-input "value: |~%~Atab~%" #\Tab))
                       nil)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)))

(it "expands a %TAG handle and suffix"
  (let* ((events (yaml-kit:parse-events
                  (test-text (format nil "%TAG !e! tag:example.com,2000:~%--- !e!foo~%"))))
         (scalar (find-if #'yaml-kit:scalar-event-p events)))
    (expect (yaml-kit:scalar-event-tag scalar)
            :to-equal "tag:example.com,2000:foo")))

(it "preserves a trailing bang in a verbatim tag"
  (let* ((events (yaml-kit:parse-events
                  (test-text (format nil "!<tag:yaml.org,2002:str!> value~%"))))
         (scalar (find-if #'yaml-kit:scalar-event-p events)))
    (expect (yaml-kit:scalar-event-tag scalar)
            :to-equal "tag:yaml.org,2002:str!")))

(defun reader-parse-outcome (text)
  (handler-case (progn (yaml-kit:parse-events text) :returned)
    (yaml-kit:yaml-parse-error () :parse-error)
    (yaml-kit:yaml-resource-limit-error () :resource-limit)
    (error (condition) condition)))

(describe "reader termination"
  (cl-weave:it-property "ends generated inputs with a parse result or a declared error"
    ((text (cl-weave:gen-string :min-length 0 :max-length 64
                                :alphabet "-?:,[]{}#&*!|>'\"%@` abcXYZ012~")))
    ;; A scanner that stops advancing enqueues tokens forever, so termination is
    ;; the property under test; the per-test timeout catches a regression.
    (let ((offenders
            (remove-if (lambda (entry)
                         (member (cdr entry)
                                 '(:returned :parse-error :resource-limit)
                                 :test #'eq))
                       (mapcar (lambda (text)
                                 (cons text (reader-parse-outcome text)))
                               (list text)))))
      (expect offenders :to-be '())))
  (it "bounds token production by the input size"
    (expect (yaml-kit::scanner-token-limit
             (yaml-kit:make-scanner (coerce "x" 'simple-string)))
            :to-be 72))
  (it "signals a resource limit when the token budget is exhausted"
    (expect (handler-case
                (progn (yaml-kit::scanner-resource-error
                        (yaml-kit:make-scanner (coerce "x" 'simple-string))
                        "tokens" 72 73)
                       nil)
              (yaml-kit:yaml-resource-limit-error (condition) condition))
            :to-be-type-of 'yaml-kit:yaml-resource-limit-error)))
