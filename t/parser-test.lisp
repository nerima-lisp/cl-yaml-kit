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

(cl-weave:defmatcher :to-signal-parser-error (actual expected)
  (let ((condition
          (handler-case
              (progn (funcall actual) nil)
            (yaml-kit:yaml-kit-error (condition) condition))))
    (multiple-value-bind (expected-value expected-valid-p)
        (%yaml-single-matcher-value expected)
      (let* ((expected-spec (if (consp expected-value)
                                expected-value
                                (list expected-value)))
             (expected-type (first expected-spec))
             (expected-message (second expected-spec))
             (expected-span (third expected-spec)))
        (values (and condition expected-valid-p
                     (typep condition expected-type)
                     (or (null expected-message)
                         (string= expected-message
                                  (cl-parser-kit:diagnostic-message
                                   (yaml-kit:yaml-kit-error-diagnostic condition))))
                     (or (null expected-span)
                         (equal expected-span
                                (cl-parser-kit:diagnostic-span
                                 (yaml-kit:yaml-kit-error-diagnostic condition)))))
                condition
                expected-spec)))))

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

(it "parses a flow sequence mapping key after a node"
  (expect (mapcar #'type-of (yaml-kit:parse-events "[a, ? b]"))
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

(it "reports missing parser nodes and invalid mapping keys"
  (expect "[a" :to-signal-yaml-error 'yaml-kit:yaml-parse-error)
  (expect "a: -" :to-signal-yaml-error 'yaml-kit:yaml-parse-error)
  (expect (mapcar #'type-of (yaml-kit:parse-events ":"))
          :to-equal
          '(yaml-kit:stream-start-event yaml-kit:document-start-event
            yaml-kit:mapping-start-event yaml-kit:scalar-event
            yaml-kit:scalar-event yaml-kit:mapping-end-event
            yaml-kit:document-end-event yaml-kit:stream-end-event)))

(it "emits stream boundaries and rejects missing node content"
  (expect (mapcar #'type-of (yaml-kit:parse-events "value"))
          :to-equal
          '(yaml-kit:stream-start-event yaml-kit:document-start-event
            yaml-kit:scalar-event yaml-kit:document-end-event
            yaml-kit:stream-end-event))
  (expect "a: -" :to-signal-yaml-error 'yaml-kit:yaml-parse-error))

(it "rejects duplicate node tags and emits the stream-end event"
  (expect "!one !two value" :to-signal-yaml-error 'yaml-kit:yaml-parse-error)
  (expect (type-of (car (last (yaml-kit:parse-events "value"))))
          :to-equal 'yaml-kit:stream-end-event))

(it "reports exact parser errors for malformed flow and block inputs"
  (expect (lambda () (yaml-kit:parse-events "[a"))
          :to-signal-parser-error
          (list 'yaml-kit:yaml-parse-error
                "did not find expected ',' or ']'"))
  (expect (lambda () (yaml-kit:parse-events #.(format nil "a: b~%- c")))
          :to-signal-parser-error
          (list 'yaml-kit:yaml-parse-error
                "did not find expected mapping key")))

(cl-weave:it-each
    (("incompatible version directive"
      #.(format nil "%YAML 2.0~%---~%value"))
     ("duplicate version directive"
      #.(format nil "%YAML 1.2~%%YAML 1.2~%---~%value"))
     ("duplicate tag directive"
      #.(format nil "%TAG !e! tag:one:~%%TAG !e! tag:two:~%---~%!e!value")))
  "rejects parser directive case ~S"
  (name input)
  (declare (ignore name))
  (expect input :to-signal-yaml-error 'yaml-kit:yaml-parse-error))
(it "parses adjustable strings"
  (let ((input (make-array 4 :element-type 'character :adjustable t
                           :initial-contents "a: 1")))
    (expect (gethash "a" (yaml-kit:parse input)) :to-equal 1))
  (expect (handler-case
              (progn (yaml-kit:parse-events "long" :max-input-length 3) nil)
            (yaml-kit:yaml-resource-limit-error () t))
          :to-be-truthy))
