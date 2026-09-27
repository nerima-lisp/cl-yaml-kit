;;;; t/reader-test.lisp
(in-package #:cl-yaml-kit/test)

(defun block-scalar-input (format-control &rest arguments)
  (let ((text (apply #'format nil format-control arguments)))
    (make-array (length text) :element-type 'character :initial-contents text)))

(defmacro block-scalar-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (let ((value (yaml-kit:parse
                                  (block-scalar-input ,(second case)))))
                      (expect (gethash ,(third case) value)
                              :to-equal ,(fourth case)))))
               cases)))

;; These are parse-level regression anchors for loader handling of block scalars.
(describe "loader block scalar regressions"
  (block-scalar-cases
    ("EMPTY-LITERAL: keeps empty scalars empty" "literal: |+~%" "literal" "")
    ("EMPTY-FOLDED: keeps empty scalars empty" "folded: >-~%" "folded" "")
    ("FOLD-CHOMP: folds and chomps independently"
     "folded: >~%  first~%  second~%~%  third~%" "folded"
     (format nil "first second~%third~%"))
    ("STRIPPED-CHOMP: strips trailing breaks"
     "stripped: |-~%  value~%~%" "stripped" "value")
    ("KEPT-CHOMP: keeps trailing breaks"
     "kept: |+~%  value~%~%" "kept" (format nil "value~%~%"))
    ("EXPLICIT-INDENT: uses the indentation indicator"
     "value: |2~%    indented~%" "value" (format nil "  indented~%")))
  (it "TAB-INDENT: rejects a tab used for block indentation"
    (expect (handler-case
                (progn (yaml-kit:parse (block-scalar-input "value: |~%~Atab~%" #\Tab))
                       nil)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)))

(defun reader-parse-outcome (text)
  (handler-case (progn (yaml-kit:parse-events text) :returned)
    (yaml-kit:yaml-parse-error () :parse-error)
    (yaml-kit:yaml-resource-limit-error () :resource-limit)
    (error (condition) condition)))

(describe "reader termination"
  (it "keeps parser event values stable after token reuse"
    (let* ((input (format nil "root:~%  first: one~%  second: two~%  third: three~%"))
           (events (yaml-kit:parse-events
                    (make-array (length input) :element-type 'character
                                :initial-contents input))))
      (let ((values (remove-if-not #'yaml-kit:scalar-event-p events)))
        (expect (mapcar #'yaml-kit:scalar-event-value values)
                :to-equal '("root" "first" "one" "second" "two"
                            "third" "three")))))
  (dolist (case
            '(("ZVH3: rejects a block sequence after a completed sibling mapping"
               "- key: value~% - item1~%")))
    (destructuring-bind (name format-control) case
      (it name
        (let* ((text (format nil format-control))
               (input (make-array (length text) :element-type 'character
                                  :initial-contents text)))
          (expect (handler-case
                      (progn (yaml-kit:parse-events input) nil)
                    (yaml-kit:yaml-parse-error (condition)
                      (string= (yaml-kit:yaml-parse-error-context condition)
                               "did not find expected node content")))
                  :to-be-truthy)))))
  (cl-weave:it-fuzz "ends generated inputs with a parse result or a declared error"
    ((text (cl-weave:gen-string :min-length 0 :max-length 64
                                :alphabet "-?:,[]{}#&*!|>'\"%@` abcXYZ012~あé")))
    (:trials 100 :timeout-per-trial 5)
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
      (unless (null offenders)
        (error "reader signaled an undeclared condition: ~S" offenders))))
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
(describe "octet input encoding"
  (flet ((event-kind (event)
           (cond ((yaml-kit:stream-start-event-p event) :stream-start)
                 ((yaml-kit:document-start-event-p event) :document-start)
                 ((yaml-kit:scalar-event-p event) :scalar)
                 ((yaml-kit:document-end-event-p event) :document-end)
                 ((yaml-kit:stream-end-event-p event) :stream-end))))
    (dolist (case
              '((:utf-8 #(#xef #xbb #xbf))
                (:utf-16be #(#xfe #xff))
                (:utf-16le #(#xff #xfe))
                (:utf-32be #(0 0 #xfe #xff))
                (:utf-32le #(#xff #xfe 0 0))))
      (destructuring-bind (encoding bom) case
        (dolist (with-bom-p '(nil t))
          (let* ((body (cl-codec-kit:string-to-octets (format nil "---~%a~%")
                                                       :encoding encoding))
                 (input (if with-bom-p
                            (concatenate '(vector (unsigned-byte 8)) bom body)
                            body))
                 (events (yaml-kit:parse-events input)))
            (expect (mapcar #'event-kind events)
                    :to-equal '(:stream-start :document-start :scalar
                                :document-end :stream-end))
            (expect (yaml-kit:scalar-event-value (third events)) :to-equal "a"))))))
  (it "rejects empty and BOM-only octet input as YAML parse errors"
    (dolist (input '(#() #(#xef #xbb #xbf) #(#xfe #xff) #(#xff #xfe)
                     #(0 0 #xfe #xff) #(#xff #xfe 0 0)))
      (expect (handler-case
                  (progn (yaml-kit:parse-events input) nil)
                (yaml-kit:yaml-parse-error () t))
              :to-be-truthy)))
  (it "turns invalid octet sequences into YAML parse errors"
    (expect (handler-case
                (progn (yaml-kit:parse-events #(#xff)) nil)
              (yaml-kit:yaml-parse-error () t)
              (error () nil))
            :to-be-truthy))
  (it "reports decode errors for typed octet input"
    (let ((input (make-array 1 :element-type '(unsigned-byte 8)
                             :initial-contents '(#xff))))
      (expect (handler-case (progn (yaml-kit:parse-events input) nil)
                (yaml-kit:yaml-parse-error () t)
                (error () nil))
              :to-be-truthy))))

(defparameter *reader-limit-cases*
  '(("scalar at limit" "value: long" :scalar)
    ("depth at limit" "[[]]" :depth)
    ("reader macro characters remain scalar text" "value: \"#.(+ 1 2)\"" :macro)))

(defun reader-test-text (text)
  (make-array (length text) :element-type 'character :initial-contents text))

(describe "reader boundary contracts"
  (it "limits a scalar at the configured boundary"
    (expect (yaml-kit:parse (reader-test-text "value: long") :max-scalar-length 5)
            :to-be-truthy))
  (it "limits parser depth"
    (expect (handler-case
                (progn (yaml-kit:parse-events (reader-test-text "[[]]") :max-depth 1) nil)
              (yaml-kit:yaml-resource-limit-error () t))
            :to-be-truthy))
  (it "keeps reader macro characters as scalar text"
    (expect (gethash "value"
                     (yaml-kit:parse (reader-test-text "value: \"#.(+ 1 2)\"")))
            :to-equal "#.(+ 1 2)"))

  (it "rejects control characters"
    (let ((input (format nil "value: ~C" (code-char 1))))
      (expect (handler-case (progn (yaml-kit:parse-events (reader-test-text input)) nil)
                (yaml-kit:yaml-parse-error () t))
              :to-be-truthy)))

  (it "limits a stream while it is being read"
    (with-input-from-string (stream "value: x")
      (expect (handler-case (progn (yaml-kit:parse-events stream :max-input-length 3) nil)
                (yaml-kit:yaml-resource-limit-error () t))
              :to-be-truthy)))

  (it "limits octets before decoding"
    (let ((input (make-array 4 :element-type '(unsigned-byte 8)
                             :initial-contents '(35 46 40 49))))
      (expect (handler-case (progn (yaml-kit:parse-events input :max-input-length 3) nil)
                (yaml-kit:yaml-resource-limit-error () t))
              :to-be-truthy)))

#+sbcl
(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defclass infinite-character-stream (sb-gray:fundamental-character-input-stream) ()))
  (defmethod sb-gray:stream-read-char ((stream infinite-character-stream))
    (declare (ignore stream))
    #\x)
  (it "stops an unending character stream at the input limit"
    (expect (handler-case
                (yaml-kit:parse-events (make-instance 'infinite-character-stream)
                                        :max-input-length 8)
              (yaml-kit:yaml-resource-limit-error () t))
            :to-be-truthy)))

)
