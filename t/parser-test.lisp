(in-package #:cl-yaml-kit/test)

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

(defun yaml-error-predicate (type message &optional span)
  (lambda (condition)
    (and (typep condition type)
         (let ((diagnostic (yaml-kit:yaml-kit-error-diagnostic condition)))
           (and (string= message
                         (cl-parser-kit:diagnostic-message diagnostic))
                (or (null span)
                    (equal span
                           (cl-parser-kit:diagnostic-span diagnostic))))))))

(cl-weave:it-each
    (("parses a scalar token stream" "hello"
      (yaml-kit:stream-start-event yaml-kit:document-start-event
        yaml-kit:scalar-event yaml-kit:document-end-event yaml-kit:stream-end-event))
     ("parses an indentless sequence as a mapping value"
      #.(format nil "a:~%- b")
      (yaml-kit:stream-start-event yaml-kit:document-start-event
        yaml-kit:mapping-start-event yaml-kit:scalar-event
        yaml-kit:sequence-start-event yaml-kit:scalar-event
        yaml-kit:sequence-end-event yaml-kit:mapping-end-event
        yaml-kit:document-end-event yaml-kit:stream-end-event))
     ("PARSER-FLOW-SEQUENCE-PAIR-NO-EXTRA-MAPPING-END" "[a: b]"
      (yaml-kit:stream-start-event yaml-kit:document-start-event
        yaml-kit:sequence-start-event yaml-kit:mapping-start-event
        yaml-kit:scalar-event yaml-kit:scalar-event yaml-kit:mapping-end-event
        yaml-kit:sequence-end-event yaml-kit:document-end-event
        yaml-kit:stream-end-event))
     ("PARSER-INDENTLESS-SEQUENCE-EMPTY-ELEMENTS" #.(format nil "a:~%-~%-")
      (yaml-kit:stream-start-event yaml-kit:document-start-event
        yaml-kit:mapping-start-event yaml-kit:scalar-event
        yaml-kit:sequence-start-event yaml-kit:scalar-event yaml-kit:scalar-event
        yaml-kit:sequence-end-event yaml-kit:mapping-end-event
        yaml-kit:document-end-event yaml-kit:stream-end-event)))
  "parses YAML event types for case ~S"
  (name input expected)
  (declare (ignore name))
  (expect (mapcar #'type-of (yaml-kit:parse-events input))
          :to-equal expected))

(it "parses an explicit key after a flow sequence separator"
  (let ((events (yaml-kit:parse-events "[a, ? b: c]")))
    (expect (mapcar #'type-of events)
            :to-equal
            '(yaml-kit:stream-start-event yaml-kit:document-start-event
              yaml-kit:sequence-start-event yaml-kit:scalar-event
              yaml-kit:mapping-start-event yaml-kit:scalar-event
              yaml-kit:scalar-event yaml-kit:mapping-end-event
              yaml-kit:sequence-end-event yaml-kit:document-end-event
              yaml-kit:stream-end-event))
    (expect (mapcar #'yaml-kit:scalar-event-value
                    (remove-if-not #'yaml-kit:scalar-event-p events))
            :to-equal
            '("a" "b" "c"))))

(it "rejects a flow sequence key without a separator"
  (expect (lambda () (yaml-kit:parse-events "[\"a\" ? b]"))
          :to-throw
          (yaml-error-predicate 'yaml-kit:yaml-parse-error
                                "did not find expected ',' or ']'")))

(cl-weave:it-each
    (("depth" "[a]" :max-depth 0
      "YAML resource limit \"depth\" exceeded: limit 0, actual 1")
     ("scalar" "long" :max-scalar-length 2
      "YAML resource limit \"scalar\" exceeded: limit 2, actual 4"))
  "enforces parser resource limit for ~S"
  (name input key value message)
  (declare (ignore name))
  (expect (lambda ()
            (apply #'yaml-kit:parse-events input (list key value)))
          :to-throw
          (yaml-error-predicate 'yaml-kit:yaml-resource-limit-error
                                message)))

(it "recognizes implicit parser tags"
  (expect (yaml-kit::parser-implicit-tag-p nil) :to-be-truthy)
  (expect (yaml-kit::parser-implicit-tag-p "") :to-be-truthy)
  (expect (yaml-kit::parser-implicit-tag-p "tag") :to-equal nil))

(cl-weave:it-each
    (("plain local tag" "!foo value" "!foo")
     ("empty local tag" "! value" "!")
     ("verbatim tag" "!<tag> value" "tag")
     ("default handle" "!!str value" "tag:yaml.org,2002:str")
     ("directive handle"
      #.(format nil "%TAG !e! tag:example:~%---~%!e!value")
      "tag:example:value"))
  "resolves parser tag input ~S"
  (name input expected)
  (declare (ignore name))
  (let* ((events (yaml-kit:parse-events input))
         (scalar (find-if #'yaml-kit:scalar-event-p events)))
    (expect (yaml-kit:scalar-event-tag scalar) :to-equal expected)))

(it "parses an explicit block mapping with an empty key"
  (let ((events (yaml-kit:parse-events #.(format nil "?~%: value"))))
    (expect (mapcar #'type-of events)
            :to-equal
            '(yaml-kit:stream-start-event yaml-kit:document-start-event
              yaml-kit:mapping-start-event yaml-kit:scalar-event
              yaml-kit:scalar-event yaml-kit:mapping-end-event
              yaml-kit:document-end-event yaml-kit:stream-end-event))
    (expect (mapcar #'yaml-kit:scalar-event-value
                    (remove-if-not #'yaml-kit:scalar-event-p events))
            :to-equal
            '("" "value"))))

(it "parses an explicit block mapping with a non-empty key"
  (let ((events (yaml-kit:parse-events #.(format nil "? key~%: value"))))
    (expect (mapcar #'type-of events)
            :to-equal
            '(yaml-kit:stream-start-event yaml-kit:document-start-event
              yaml-kit:mapping-start-event yaml-kit:scalar-event
              yaml-kit:scalar-event yaml-kit:mapping-end-event
              yaml-kit:document-end-event yaml-kit:stream-end-event))
    (expect (mapcar #'yaml-kit:scalar-event-value
                    (remove-if-not #'yaml-kit:scalar-event-p events))
            :to-equal
            '("key" "value"))))

(it "emits stream boundaries for a scalar"
  (expect (mapcar #'type-of (yaml-kit:parse-events "value"))
          :to-equal
          '(yaml-kit:stream-start-event yaml-kit:document-start-event
            yaml-kit:scalar-event yaml-kit:document-end-event
            yaml-kit:stream-end-event)))

(it "rejects a block sequence entry after a mapping value in the scanner"
  (expect (lambda () (yaml-kit:parse-events "a: -"))
          :to-throw
          (yaml-error-predicate 'yaml-kit:yaml-parse-error
                                "block sequence entries are not allowed in this context")))

(it "rejects duplicate node tags and emits the stream-end event"
  (expect (lambda () (yaml-kit:parse-events "!one !two value"))
          :to-throw
          (yaml-error-predicate 'yaml-kit:yaml-parse-error
                                "did not find expected node content"))
  (expect (type-of (car (last (yaml-kit:parse-events "value"))))
          :to-equal
          'yaml-kit:stream-end-event))

(it "reports exact parser errors for malformed flow and block inputs"
  (expect (lambda () (yaml-kit:parse-events "[a"))
          :to-throw
          (yaml-error-predicate 'yaml-kit:yaml-parse-error
                                "did not find expected ',' or ']'"))
  (expect (lambda ()
            (yaml-kit:parse-events #.(format nil "a: b~%- c")))
          :to-throw
          (yaml-error-predicate 'yaml-kit:yaml-parse-error
                                "did not find expected mapping key")))

(cl-weave:it-each
    (("incompatible version directive"
      #.(format nil "%YAML 2.0~%---~%value")
      "found incompatible YAML major version")
     ("duplicate version directive"
      #.(format nil "%YAML 1.2~%%YAML 1.2~%---~%value")
      "found duplicate %YAML directive")
     ("duplicate tag directive"
      #.(format nil "%TAG !e! tag:one:~%%TAG !e! tag:two:~%---~%!e!value")
      "found duplicate %TAG directive"))
  "rejects parser directive case ~S"
  (name input message)
  (declare (ignore name))
  (expect (lambda () (yaml-kit:parse-events input))
          :to-throw
          (yaml-error-predicate 'yaml-kit:yaml-parse-error
                                message)))

(it "parses adjustable strings"
  (let ((input (make-array 4 :element-type 'character :adjustable t
                           :initial-contents "a: 1")))
    (expect (gethash "a" (yaml-kit:parse input)) :to-equal 1)))

(it "rejects strings exceeding max input length"
  (expect (lambda ()
            (yaml-kit:parse-events "long" :max-input-length 3))
          :to-throw
          (yaml-error-predicate 'yaml-kit:yaml-resource-limit-error
                                "YAML resource limit \"input length\" exceeded: limit 3, actual 4")))

(it "preserves the first unread character after input length rejection"
  (let ((stream (make-string-input-stream "long!")))
    (expect (lambda ()
              (yaml-kit:parse-events stream :max-input-length 3))
            :to-throw
            (yaml-error-predicate 'yaml-kit:yaml-resource-limit-error
                                  "YAML resource limit \"input length\" exceeded: limit 3, actual 4"))
    (expect (read-char stream nil nil) :to-equal (char "!" 0))))

(it "returns only primary parser continuation values"
  (let ((continuation (lambda (value)
                       (declare (ignore value))
                       (values :primary :secondary))))
    (expect (multiple-value-list
             (yaml-kit:map-events/k
              (lambda (event) (declare (ignore event)))
              "value"
              continuation))
            :to-equal
            '(:primary))
    (expect (multiple-value-list
             (yaml-kit:parse-events/k "value" continuation))
            :to-equal
            '(:primary))))

(cl-weave:it-property
 "preserves generated plain scalar values in parser events"
 ((text (cl-weave:gen-string :min-length 1 :max-length 32
                             :alphabet "abcXYZ012あé")))
 (let* ((events (yaml-kit:parse-events text))
        (scalars (remove-if-not #'yaml-kit:scalar-event-p events)))
   (expect (length scalars) :to-equal 1)
   (expect (yaml-kit:scalar-event-value (first scalars)) :to-equal text)
   (expect (list (type-of (first events))
                 (type-of (car (last events))))
           :to-equal
           '(yaml-kit:stream-start-event yaml-kit:stream-end-event))))

(cl-weave:it-fuzz
 "reports YAML errors for generated unterminated flow sequences"
 ((text (cl-weave:gen-string :min-length 0 :max-length 32
                             :alphabet "abcXYZ012")))
 (:trials 100 :timeout-per-trial 5)
 (expect (lambda () (yaml-kit:parse-events (format nil "[~A" text)))
         :to-throw
         'yaml-kit:yaml-parse-error))
