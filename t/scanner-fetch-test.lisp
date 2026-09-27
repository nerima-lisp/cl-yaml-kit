(in-package #:cl-yaml-kit/test)

(defmacro scanner-fetch-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (let* ((text (second ',case))
                           (source (make-array (length text)
                                               :element-type 'character
                                               :initial-contents text))
                           (scanner (yaml-kit:make-scanner source))
                           (actual nil))
                      (loop for token = (yaml-kit:scanner-next-token scanner)
                            while token do
                              (push (yaml-kit:token-kind token) actual))
                      (expect (nreverse actual) :to-equal (third ',case)))))
               cases)))

(scanner-fetch-cases
  ("block sequence" "- a" (:stream-start :block-sequence-start :block-entry :scalar :block-end :stream-end))
  ("nested block sequence" "- - a" (:stream-start :block-sequence-start :block-entry :block-sequence-start :block-entry :scalar :block-end :block-end :stream-end))
  ("simple mapping" "a: b" (:stream-start :block-mapping-start :key :scalar :value :scalar :block-end :stream-end))
  ("complex mapping key" "? a : b" (:stream-start :block-mapping-start :key :scalar :value :scalar :block-end :stream-end))
  ("flow sequence" "[a, b]" (:stream-start :flow-sequence-start :scalar :flow-entry :scalar :flow-sequence-end :stream-end))
  ("flow mapping" "{a: b}" (:stream-start :flow-mapping-start :key :scalar :value :scalar :flow-mapping-end :stream-end))
  ("flow sequence pair" "[a: b]" (:stream-start :flow-sequence-start :key :scalar :value :scalar :flow-sequence-end :stream-end))
  ("anchor" "&x a" (:stream-start :anchor :scalar :stream-end))
  ("alias" "*x" (:stream-start :alias :stream-end))
  ("tag" "!t a" (:stream-start :tag :scalar :stream-end))
  ("literal scalar" "|\na\n" (:stream-start :scalar :stream-end))
  ("folded scalar" ">\na\n" (:stream-start :scalar :stream-end))
  ("single quoted scalar" "'a'" (:stream-start :scalar :stream-end))
  ("double quoted scalar" "\"a\"" (:stream-start :scalar :stream-end))
  ;; The "..." sits in column 6, so libyaml scans it as plain scalar content
  ;; (scanner.c requires column zero for a document end) and the scalar is
  ;; "a ...".  Tokenising it as a document end once stalled the scanner.
  ("document indicators" "--- a ..." (:stream-start :document-start :scalar :stream-end))
  ("document end in flow context" "[
...
]"
   (:stream-start :flow-sequence-start :document-end :flow-sequence-end :stream-end))
  ("indented document-looking plain scalar in flow context" "[
  ...
]"
   (:stream-start :flow-sequence-start :scalar :flow-sequence-end :stream-end)))

(describe "scanner fetch errors"
  (it "rejects a block entry after a scalar"
    (expect (handler-case
                (progn
                  (let ((scanner (yaml-kit:make-scanner (coerce "a -" 'simple-string))))
                    (loop while (yaml-kit:scanner-next-token scanner)))
                  nil)
              (yaml-kit:yaml-parse-error () t)) :to-be-truthy))
  (it "rejects a block key after a scalar"
    (expect (handler-case
                (progn
                  (let ((scanner (yaml-kit:make-scanner (coerce "a ?" 'simple-string))))
                    (loop while (yaml-kit:scanner-next-token scanner)))
                  nil)
              (yaml-kit:yaml-parse-error () t)) :to-be-truthy))
  (it "rejects a block value after a scalar"
    (expect (handler-case
                (progn
                  (let ((scanner (yaml-kit:make-scanner (coerce "a :" 'simple-string))))
                    (loop while (yaml-kit:scanner-next-token scanner)))
                  nil)
              (yaml-kit:yaml-parse-error () t)) :to-be-truthy)))
