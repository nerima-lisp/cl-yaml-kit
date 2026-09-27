(in-package #:cl-yaml-kit/test)

(defstruct parser-test-scanner tokens)

(defun parser-test-peek (scanner)
  (car (parser-test-scanner-tokens scanner)))

(defun parser-test-next (scanner)
  (pop (parser-test-scanner-tokens scanner)))

(defun parse-parser-token-events (specs)
  (let* ((mark (yaml-kit:make-mark 1 1 0))
         (scanner (make-parser-test-scanner
                   :tokens (mapcar (lambda (spec)
                                     (apply #'yaml-kit:make-token
                                            (first spec) mark mark (rest spec)))
                                   specs)))
         (events nil)
         (parser (yaml-kit::make-parser%
                  :scanner scanner
                  :state #'yaml-kit::yaml-parser-parse-stream-start
                  :handler (lambda (event) (push event events))
                  :depth 0 :max-depth 256 :max-scalar-length 1000
                  :peek-function #'parser-test-peek
                  :next-function #'parser-test-next)))
    (loop for state = (yaml-kit::parser-state parser) while state do
      (setf (yaml-kit::parser-state parser) (funcall state parser)))
    (nreverse events)))

(defun parser-event-signature (event)
  (cond
    ((yaml-kit:sequence-start-event-p event)
     (list :sequence-start
           (yaml-kit:sequence-start-event-implicit-p event)
           (yaml-kit:sequence-start-event-style event)
           (yaml-kit:sequence-start-event-anchor event)
           (yaml-kit:sequence-start-event-tag event)))
    ((yaml-kit:mapping-start-event-p event)
     (list :mapping-start
           (yaml-kit:mapping-start-event-implicit-p event)
           (yaml-kit:mapping-start-event-style event)
           (yaml-kit:mapping-start-event-anchor event)
           (yaml-kit:mapping-start-event-tag event)))
    ((yaml-kit:scalar-event-p event)
     (list :scalar
           (yaml-kit:scalar-event-value event)
           (yaml-kit:scalar-event-anchor event)
           (yaml-kit:scalar-event-tag event)
           (yaml-kit:scalar-event-plain-implicit-p event)
           (yaml-kit:scalar-event-quoted-implicit-p event)))
    (t (type-of event))))

(defmacro parser-token-cases (&body cases)
  `(progn
     ,@(mapcar
        (lambda (case)
          `(it ,(first case)
             (expect (mapcar #'type-of
                             (parse-parser-token-events ',(second case)))
                     :to-equal ',(third case))))
        cases)))

(parser-token-cases
  ("parses a scalar token stream"
   ((:stream-start) (:scalar :value "hello" :style :plain) (:stream-end))
   (yaml-kit:stream-start-event yaml-kit:document-start-event
    yaml-kit:scalar-event yaml-kit:document-end-event yaml-kit:stream-end-event))
  ("parses an indentless sequence as a mapping value"
   ((:stream-start) (:block-mapping-start) (:key) (:scalar :value "a" :style :plain)
    (:value) (:block-entry) (:scalar :value "b" :style :plain) (:block-end)
    (:stream-end))
   (yaml-kit:stream-start-event yaml-kit:document-start-event
    yaml-kit:mapping-start-event yaml-kit:scalar-event
    yaml-kit:sequence-start-event yaml-kit:scalar-event
    yaml-kit:sequence-end-event yaml-kit:mapping-end-event
    yaml-kit:document-end-event yaml-kit:stream-end-event))
  ("PARSER-FLOW-SEQUENCE-PAIR-NO-EXTRA-MAPPING-END"
   ((:stream-start) (:flow-sequence-start) (:key) (:scalar :value "a" :style :plain)
    (:value) (:scalar :value "b" :style :plain) (:flow-sequence-end) (:stream-end))
   (yaml-kit:stream-start-event yaml-kit:document-start-event
    yaml-kit:sequence-start-event yaml-kit:mapping-start-event
    yaml-kit:scalar-event yaml-kit:scalar-event yaml-kit:mapping-end-event
    yaml-kit:sequence-end-event yaml-kit:document-end-event
    yaml-kit:stream-end-event))
  ("PARSER-INDENTLESS-SEQUENCE-EMPTY-ELEMENTS"
   ((:stream-start) (:block-mapping-start) (:key) (:scalar :value "a" :style :plain)
    (:value) (:block-entry) (:block-entry) (:block-end) (:stream-end))
   (yaml-kit:stream-start-event yaml-kit:document-start-event
    yaml-kit:mapping-start-event yaml-kit:scalar-event
    yaml-kit:sequence-start-event yaml-kit:scalar-event yaml-kit:scalar-event
    yaml-kit:sequence-end-event yaml-kit:mapping-end-event
    yaml-kit:document-end-event yaml-kit:stream-end-event)))

(it "expands parser states and defines callable state functions"
  (let ((expansion
          (macroexpand-1
           '(yaml-kit::define-parser-state probe-parser-state (parser)
              (declare (ignore parser))
              :probe))))
    (expect (first expansion) :to-equal 'defun))
  (eval '(yaml-kit::define-parser-state probe-parser-state-eval (parser)
           (declare (ignore parser))
           :probe))
  (let ((state (find-symbol "PROBE-PARSER-STATE-EVAL" :cl-yaml-kit/test)))
    (expect (funcall (symbol-function state) nil) :to-equal :probe)))

(it "exercises parser state helpers with custom and scanner callbacks"
  (declare (notinline yaml-kit::parser-peek yaml-kit::parser-next
                      yaml-kit::parser-push yaml-kit::parser-pop
                      yaml-kit::parser-emit))
  (let* ((scanner (yaml-kit::make-scanner "x"))
         (events nil)
         (parser (yaml-kit::make-parser%
                  :scanner scanner :states nil :handler (lambda (event) (push event events))
                  :peek-function (lambda (s) (declare (ignore s)) :custom-peek)
                  :next-function (lambda (s) (declare (ignore s)) :custom-next))))
    (expect (yaml-kit::parser-peek parser) :to-equal :custom-peek)
    (expect (yaml-kit::parser-next parser) :to-equal :custom-next)
    (setf (yaml-kit::parser-peek-function parser) nil
          (yaml-kit::parser-next-function parser) nil)
    (expect (yaml-kit::parser-peek parser) :to-be-truthy)
    (expect (yaml-kit::parser-next parser) :to-be-truthy)
    (yaml-kit::parser-push parser #'identity)
    (expect (length (yaml-kit::parser-states parser)) :to-equal 1)
    (expect (funcall (yaml-kit::parser-pop parser) :value) :to-equal :value)
    (expect (funcall (yaml-kit::parser-pop parser) parser) :to-equal nil)
    (expect (yaml-kit::parser-emit parser :event) :to-equal nil)
    (expect events :to-equal '(:event))))

(describe "parser event details"
  (macrolet ((parser-event-cases (&body cases)
               `(progn
                  ,@(mapcar (lambda (case)
                              `(it ,(first case)
                                 (expect (parser-event-signature
                                          (nth 2 (yaml-kit:parse-events ,(second case))))
                                         :to-equal ',(third case))))
                            cases))))
    ;; c-ns-properties makes a tagged collection explicit, so implicit-p is off.
    (parser-event-cases
      ("PARSER-FLOW-SEQUENCE-START-IMPLICIT" "[a]"
       (:sequence-start t :flow nil nil))
      ("PARSER-FLOW-MAPPING-START-IMPLICIT" "{a: b}"
       (:mapping-start t :flow nil nil))
      ("PARSER-FLOW-SEQUENCE-START-EXPLICIT-TAG" "!!seq [a]"
       (:sequence-start nil :flow nil "tag:yaml.org,2002:seq"))
      ("PARSER-FLOW-MAPPING-START-EXPLICIT-TAG" "!!map {a: b}"
       (:mapping-start nil :flow nil "tag:yaml.org,2002:map"))))
  (it "PARSER-EMPTY-ANCHOR-SCALAR-PRESERVES-PROPERTIES"
    (expect (parser-event-signature
             (nth 2 (parse-parser-token-events
                     '((:stream-start) (:anchor :value "a") (:stream-end)))))
            :to-equal '(:scalar "" "a" nil t nil)))
  (it "PARSER-FLOW-NODE-EXPECTED-CONTENT"
    (expect (handler-case
                (progn
                  (parse-parser-token-events
                   '((:stream-start) (:flow-sequence-start)
                     (:block-sequence-start) (:block-end)
                     (:flow-sequence-end) (:stream-end)))
                  nil)
            (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)))
  (dolist (case '(("flow sequence key after entry" "[a, ? b]")
                  ("flow sequence empty mapping value" "[a:]")
                  ("flow sequence missing value indicator" "[? a]")
                  ("flow sequence empty mapping key" "[?]")))
    (destructuring-bind (name input) case
      (it name
        (expect (handler-case (yaml-kit:parse-events input)
                  (yaml-kit:yaml-parse-error () nil))
                :to-be-truthy))))
