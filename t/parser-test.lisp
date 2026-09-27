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

(it "parses a flow sequence mapping key after a node"
  (expect (mapcar #'type-of
                  (parse-parser-token-events
                   '((:stream-start) (:flow-sequence-start)
                     (:scalar :value "a" :style :plain) (:key)
                     (:scalar :value "b" :style :plain)
                     (:flow-sequence-end) (:stream-end))))
          :to-equal
          '(yaml-kit:stream-start-event yaml-kit:document-start-event
            yaml-kit:sequence-start-event yaml-kit:scalar-event
            yaml-kit:mapping-start-event yaml-kit:scalar-event
            yaml-kit:scalar-event yaml-kit:mapping-end-event
            yaml-kit:mapping-end-event yaml-kit:sequence-end-event
            yaml-kit:document-end-event yaml-kit:stream-end-event)))

(it "enforces parser depth and scalar resource limits"
  (dolist (case '(("depth" "[a]" :max-depth 0)
                  ("scalar" "long" :max-scalar-length 2)))
    (destructuring-bind (name input key value) case
      (declare (ignore name))
      (expect (handler-case (progn (apply #'yaml-kit:parse-events input (list key value)) nil)
                (yaml-kit:yaml-resource-limit-error () t))
              :to-be-truthy))))

(it "covers parser tag token forms and implicit tag checks"
  (expect (funcall (symbol-function 'yaml-kit::parser-implicit-tag-p) nil) :to-be-truthy)
  (expect (funcall (symbol-function 'yaml-kit::parser-implicit-tag-p) "") :to-be-truthy)
  (expect (funcall (symbol-function 'yaml-kit::parser-implicit-tag-p) "tag") :to-equal nil)
  (let* ((mark (yaml-kit:make-mark 0 0 0))
         (parser (yaml-kit::make-parser%
                  :directives '(("!" . "!") ("!!" . "tag:yaml.org,2002:")
                                ("!e!" . "tag:example:")))))
    (dolist (case '((nil "plain") ("!" "suffix") ("!" "<tag>") ("!e!" "value")))
      (destructuring-bind (handle suffix) case
        (expect (funcall (symbol-function 'yaml-kit::parser-tag-token)
                         parser (yaml-kit:make-token :tag mark mark :handle handle :suffix suffix))
                :to-be-truthy)))))

(it "reports missing parser nodes and invalid mapping keys"
  (progn
    (expect (handler-case
                (yaml-kit::parser-node
                 (yaml-kit::make-parser%
                  :scanner (yaml-kit::make-scanner ""))
                 t nil)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)
    (expect (handler-case
                (parse-parser-token-events
                 '((:stream-start) (:block-mapping-start) (:block-entry) (:stream-end)))
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)
    (expect (mapcar #'type-of
                    (parse-parser-token-events
                     '((:stream-start) (:block-mapping-start) (:key)
                       (:block-end) (:stream-end))))
            :to-equal
            '(yaml-kit:stream-start-event yaml-kit:document-start-event
              yaml-kit:mapping-start-event yaml-kit:scalar-event
              yaml-kit:scalar-event yaml-kit:mapping-end-event
              yaml-kit:document-end-event yaml-kit:stream-end-event))))

(it "reports missing stream-start and node tokens directly"
  (let ((parser (yaml-kit::make-parser%
                 :peek-function (lambda (scanner) (declare (ignore scanner)) nil)
                 :next-function (lambda (scanner) (declare (ignore scanner)) nil))))
    (expect (handler-case
                (funcall (symbol-function 'yaml-kit::yaml-parser-parse-stream-start)
                         parser)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)
    (expect (handler-case (yaml-kit::parser-node parser nil nil)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)))

(it "rejects duplicate node tags and invalid stream-end tokens"
  (expect (handler-case
              (parse-parser-token-events
               '((:stream-start) (:tag :handle "!" :suffix "one")
                 (:tag :handle "!" :suffix "two")
                 (:scalar :value "value" :style :plain) (:stream-end)))
            (yaml-kit:yaml-parse-error () t))
          :to-be-truthy)
  (let* ((mark (yaml-kit:make-mark 0 0 0))
         (token (yaml-kit:make-token :scalar mark mark :value "wrong" :style :plain))
         (parser (yaml-kit::make-parser%
                  :next-function (lambda (scanner) (declare (ignore scanner)) token)
                  :handler (lambda (event) (declare (ignore event))))))
    (expect (handler-case
                (funcall (symbol-function 'yaml-kit::yaml-parser-parse-stream-end) parser)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)))

(it "rejects incompatible and duplicate parser directives"
  (dolist (tokens '((( :stream-start) (:version-directive :major 2 :minor 0)
                     (:document-start) (:stream-end))
                    ((:stream-start) (:version-directive :major 1 :minor 2)
                     (:version-directive :major 1 :minor 2) (:document-start)
                     (:stream-end))
                    ((:stream-start) (:tag-directive :handle "!e!" :value "tag:one:")
                     (:tag-directive :handle "!e!" :value "tag:two:")
                     (:document-start) (:stream-end))))
    (expect (handler-case (parse-parser-token-events tokens)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)))
