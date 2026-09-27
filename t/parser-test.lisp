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
                  :depth 0 :max-depth 256 :max-scalar-length 1000)))
    (let ((old-peek (symbol-function 'yaml-kit:scanner-peek-token))
          (old-next (symbol-function 'yaml-kit:scanner-next-token)))
      (unwind-protect
           (progn
             (setf (symbol-function 'yaml-kit:scanner-peek-token) #'parser-test-peek
                   (symbol-function 'yaml-kit:scanner-next-token) #'parser-test-next)
             (loop for state = (yaml-kit::parser-state parser) while state do
               (setf (yaml-kit::parser-state parser) (funcall state parser))))
        (setf (symbol-function 'yaml-kit:scanner-peek-token) old-peek
              (symbol-function 'yaml-kit:scanner-next-token) old-next)))
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

(describe "parser event details"
  (it "PARSER-FLOW-SEQUENCE-START-IMPLICIT"
    (expect (parser-event-signature
             (nth 2 (yaml-kit:parse-events "[a]")))
            :to-equal '(:sequence-start t :flow nil nil)))
  (it "PARSER-FLOW-MAPPING-START-IMPLICIT"
    (expect (parser-event-signature
             (nth 2 (yaml-kit:parse-events "{a: b}")))
            :to-equal '(:mapping-start t :flow nil nil)))
  (it "PARSER-FLOW-SEQUENCE-START-EXPLICIT-TAG"
    (expect (parser-event-signature
             (nth 2 (yaml-kit:parse-events "!!seq [a]")))
            :to-equal '(:sequence-start t :flow nil "tag:yaml.org,2002:seq")))
  (it "PARSER-FLOW-MAPPING-START-EXPLICIT-TAG"
    (expect (parser-event-signature
             (nth 2 (yaml-kit:parse-events "!!map {a: b}")))
            :to-equal '(:mapping-start t :flow nil "tag:yaml.org,2002:map")))
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
