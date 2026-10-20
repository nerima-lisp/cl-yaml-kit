(in-package #:cl-yaml-kit/test)

(cl-weave:it-each
    ((block-sequence "- a"
       (:stream-start :block-sequence-start :block-entry :scalar :block-end :stream-end))
     (nested-block-sequence "- - a"
       (:stream-start :block-sequence-start :block-entry :block-sequence-start
        :block-entry :scalar :block-end :block-end :stream-end))
     (simple-mapping "a: b"
       (:stream-start :block-mapping-start :key :scalar :value :scalar :block-end :stream-end))
     ;; A compact mapping introduces its own block mapping beneath an explicit key.
     (complex-mapping-key "? a : b"
       (:stream-start :block-mapping-start :key :block-mapping-start :key :scalar
        :value :scalar :block-end :block-end :stream-end))
     (flow-sequence "[a, b]"
       (:stream-start :flow-sequence-start :scalar :flow-entry :scalar
        :flow-sequence-end :stream-end))
     (flow-mapping "{a: b}"
       (:stream-start :flow-mapping-start :key :scalar :value :scalar
        :flow-mapping-end :stream-end))
     (flow-sequence-pair "[a: b]"
       (:stream-start :flow-sequence-start :key :scalar :value :scalar
        :flow-sequence-end :stream-end))
     (anchor "&x a" (:stream-start :anchor :scalar :stream-end))
     (alias "*x" (:stream-start :alias :stream-end))
     (tag "!t a" (:stream-start :tag :scalar :stream-end))
     (literal-scalar #.(format nil "|~%a~%") (:stream-start :scalar :stream-end))
     (folded-scalar #.(format nil ">~%a~%") (:stream-start :scalar :stream-end))
     (single-quoted-scalar "'a'" (:stream-start :scalar :stream-end))
     (double-quoted-scalar "\"a\"" (:stream-start :scalar :stream-end))
     (document-indicators "--- a ..." (:stream-start :document-start :scalar :stream-end))
     (document-end-in-flow-context #.(format nil "[~%...~%]")
       (:stream-start :flow-sequence-start :document-end :flow-sequence-end :stream-end))
     (indented-document-looking-scalar-in-flow-context #.(format nil "[~%  ...~%]")
       (:stream-start :flow-sequence-start :scalar :flow-sequence-end :stream-end)))
  "scans ~S into the expected token kinds"
  (name text expected)
  (declare (ignore name))
  (expect (scanner-token-kinds text) :to-equal expected))

(cl-weave:it-each
    ((block-entry-after-value "a: -")
     (block-key-after-value "a: ?")
     (block-value-after-value "a: :"))
  ;; After a mapping value indicator, another block indicator cannot start the
  ;; value node that the mapping announced.
  "signals a YAML parse error when scanning ~S"
  (name text)
  (declare (ignore name))
  (expect
    (lambda () (scanner-token-kinds text))
    :to-throw
    'yaml-kit:yaml-parse-error))

(defun scanner-token-offsets (text)
  (let ((scanner (yaml-kit::make-scanner (scanner-source text))))
    (loop for token = (yaml-kit::%scanner-next-token scanner)
          while token
          collect (list (cl-parser-kit:span-start (yaml-kit:token-start-span token))
                        (cl-parser-kit:span-end (yaml-kit:token-end-span token))))))

(defun scanner-token-offsets-valid-p (offsets text-length)
  (and (consp offsets)
       (loop with previous-end = 0
             for offset in offsets
             for start = (first offset)
             for end = (second offset)
             always (and (integerp start)
                         (integerp end)
                         (<= previous-end start end text-length))
             do (setf previous-end end))
       (= (second (car (last offsets))) text-length)))

(cl-weave:it-property
 "preserves generated plain scalar values and endpoint offsets"
 ((text (cl-weave:gen-string :min-length 1 :max-length 32
                             :alphabet "abcXYZ012")))
 (let* ((scanner (yaml-kit::make-scanner (scanner-source text)))
        (scalar (loop for token = (yaml-kit::%scanner-next-token scanner)
                      while token
                      when (eq (yaml-kit:token-kind token) :scalar)
                        return token)))
   (expect (list (yaml-kit:token-value scalar)
                 (cl-parser-kit:span-start (yaml-kit:token-start-span scalar))
                 (cl-parser-kit:span-end (yaml-kit:token-end-span scalar)))
           :to-equal (list text 0 (length text)))))

(cl-weave:it-fuzz "classifies generated scanner inputs and preserves token progress"
  ((text (cl-weave:gen-string :min-length 0 :max-length 64
                              :alphabet "-?:,[]{}#&*!|>'\"%@` abcXYZ012~あé")))
  (:trials 100 :timeout-per-trial 5)
  (let ((outcome
          (handler-case
              (cons :tokens (scanner-token-offsets text))
            (error (condition)
              (cons :condition condition)))))
    (ecase (car outcome)
      (:tokens
       (expect (scanner-token-offsets-valid-p (cdr outcome) (length text))
               :to-be-truthy))
      (:condition
       (expect
         (lambda () (error (cdr outcome)))
         :to-throw
         (lambda (condition)
           (and (typep condition 'yaml-kit:yaml-parse-error)
                (plusp
                 (length
                  (cl-parser-kit:diagnostic-message
                   (yaml-kit:yaml-kit-error-diagnostic condition)))))))))))
