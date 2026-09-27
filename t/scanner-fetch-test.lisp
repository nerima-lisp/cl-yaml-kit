(in-package #:cl-yaml-kit/test)

;; The expected token lists are quoted: this SBCL build miscompiles a literal
;; list that appears directly as a function argument, and "~%" is how a line
;; break gets into a string because the reader here does not expand "\n".
(defparameter *scanner-fetch-cases*
  (list
    (list "block sequence" "- a"
          '(:stream-start :block-sequence-start :block-entry :scalar :block-end :stream-end))
    (list "nested block sequence" "- - a"
          '(:stream-start :block-sequence-start :block-entry :block-sequence-start
            :block-entry :scalar :block-end :block-end :stream-end))
    (list "simple mapping" "a: b"
          '(:stream-start :block-mapping-start :key :scalar :value :scalar :block-end :stream-end))
    ;; "? a : b" opens a block mapping for the key "a", because a compact mapping
    ;; rolls its own indentation, so the outer mapping only supplies a KEY.
    (list "complex mapping key" "? a : b"
          '(:stream-start :block-mapping-start :key :block-mapping-start :key :scalar
            :value :scalar :block-end :block-end :stream-end))
    (list "flow sequence" "[a, b]"
          '(:stream-start :flow-sequence-start :scalar :flow-entry :scalar :flow-sequence-end :stream-end))
    (list "flow mapping" "{a: b}"
          '(:stream-start :flow-mapping-start :key :scalar :value :scalar :flow-mapping-end :stream-end))
    (list "flow sequence pair" "[a: b]"
          '(:stream-start :flow-sequence-start :key :scalar :value :scalar :flow-sequence-end :stream-end))
    (list "anchor" "&x a" '(:stream-start :anchor :scalar :stream-end))
    (list "alias" "*x" '(:stream-start :alias :stream-end))
    (list "tag" "!t a" '(:stream-start :tag :scalar :stream-end))
    (list "literal scalar" (format nil "|~%a~%") '(:stream-start :scalar :stream-end))
    (list "folded scalar" (format nil ">~%a~%") '(:stream-start :scalar :stream-end))
    (list "single quoted scalar" "'a'" '(:stream-start :scalar :stream-end))
    (list "double quoted scalar" "\"a\"" '(:stream-start :scalar :stream-end))
    ;; The "..." sits in column 6, so libyaml scans it as plain scalar content
    ;; (scanner.c requires column zero for a document end) and the scalar is
    ;; "a ...".  Tokenising it as a document end once stalled the scanner.
    (list "document indicators" "--- a ..." '(:stream-start :document-start :scalar :stream-end))
    (list "document end in flow context" (format nil "[~%...~%]")
          '(:stream-start :flow-sequence-start :document-end :flow-sequence-end :stream-end))
    (list "indented document-looking plain scalar in flow context" (format nil "[~%  ...~%]")
          '(:stream-start :flow-sequence-start :scalar :flow-sequence-end :stream-end))))

(defmacro scanner-fetch-cases (table)
  `(dolist (case ,table)
     (it (first case)
       (let ((scanner (yaml-kit:make-scanner (scanner-source (second case))))
             (kinds nil))
         (loop for token = (yaml-kit:scanner-next-token scanner)
               while token
               do (push (yaml-kit:token-kind token) kinds))
         (expect (nreverse kinds) :to-equal (third case))))))

(describe "scanner fetch"
  (scanner-fetch-cases *scanner-fetch-cases*))

(describe "scanner fetch errors"
  ;; A value indicator leaves simple-key-allowed off, so a further block
  ;; indicator cannot start the value node it announced.
  (macrolet ((scanner-error-cases (&body cases)
               `(progn
                  ,@(mapcar (lambda (case)
                              `(it ,(first case)
                                 (expect (handler-case
                                             (progn
                                               (let ((scanner (yaml-kit:make-scanner
                                                               (scanner-source ,(second case)))))
                                                 (loop while (yaml-kit:scanner-next-token scanner)))
                                               nil)
                                           (yaml-kit:yaml-parse-error () t))
                                         :to-be-truthy)))
                            cases))))
    (scanner-error-cases
      ("rejects a block entry after a value indicator" "a: -")
      ("rejects a block key after a value indicator" "a: ?")
      ("rejects a block value after a value indicator" "a: :"))))
