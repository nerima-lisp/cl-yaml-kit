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
                  "%TAG !e! tag:example.com,2000:\n--- !e!foo\n"))
         (scalar (find-if #'yaml-kit:scalar-event-p events)))
    (expect (yaml-kit:scalar-event-tag scalar)
            :to-equal "tag:example.com,2000:foo")))

(it "preserves a trailing bang in a verbatim tag"
  (let* ((events (yaml-kit:parse-events "!<tag:yaml.org,2002:str!> value\n"))
         (scalar (find-if #'yaml-kit:scalar-event-p events)))
    (expect (yaml-kit:scalar-event-tag scalar)
            :to-equal "tag:yaml.org,2002:str!")))
