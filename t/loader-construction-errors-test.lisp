(in-package #:cl-yaml-kit/test)

(describe "loader construction errors"
  (loader-error-cases
    ("rejects a collection tag on a scalar"
     (loader-parse-events
      (loader-document (loader-event :scalar :tag "!!seq" :value "value")))
     yaml-kit:yaml-compose-error)
    ("rejects a float with trailing data"
     (loader-parse-events
      (loader-document (loader-event :scalar :tag "!!float" :value "1 2")))
     yaml-kit:yaml-compose-error)
    ("rejects an unknown alias"
     (loader-compose-events
      (loader-document (loader-event :alias :anchor "missing")))
     yaml-kit:yaml-compose-error)
    ("rejects two root nodes"
     (loader-compose-events
      (loader-document
       (loader-event :scalar :value "one")
       (loader-event :scalar :value "two")))
     yaml-kit:yaml-compose-error)
    ("rejects an unmatched collection end"
     (loader-compose-events
      (loader-document (loader-event :sequence-end)))
     yaml-kit:yaml-compose-error)
    ("rejects an odd mapping"
     (loader-compose-events
      (loader-document
       (loader-event :mapping-start)
       (loader-event :scalar :value "key")
       (loader-event :mapping-end)))
     yaml-kit:yaml-compose-error)
    ("rejects an invalid mapping type"
     (loader-parse-events (loader-document (loader-event :scalar :value "x"))
                          :mapping-type :invalid)
     yaml-kit:yaml-compose-error)
    ("rejects an invalid sequence type"
     (loader-parse-events (loader-document (loader-event :scalar :value "x"))
                          :sequence-type :invalid)
     yaml-kit:yaml-compose-error)
    ("rejects an invalid duplicate policy"
     (loader-parse-events (loader-document (loader-event :scalar :value "x"))
                          :duplicate-key-policy :invalid)
     yaml-kit:yaml-compose-error)
    ("rejects a malformed integer tag"
     (loader-parse-events
      (loader-document (loader-event :scalar :tag "!!int" :value "nope")))
     yaml-kit:yaml-compose-error)
    ("rejects an empty integer tag"
     (loader-parse-events
      (loader-document (loader-event :scalar :tag "!!int" :value "")))
     yaml-kit:yaml-compose-error)
    ("rejects a malformed boolean tag"
     (loader-parse-events
      (loader-document (loader-event :scalar :tag "!!bool" :value "maybe")))
     yaml-kit:yaml-compose-error)
    ("rejects a malformed null tag"
     (loader-parse-events
      (loader-document (loader-event :scalar :tag "!!null" :value "nope")))
     yaml-kit:yaml-compose-error)
    ("rejects a non-scalar hash key"
     (loader-parse-events
      (loader-document
       (loader-event :mapping-start)
       (loader-event :sequence-start)
       (loader-event :sequence-end)
       (loader-event :scalar :value "value")
       (loader-event :mapping-end)))
     yaml-kit:yaml-compose-error)
    ("rejects a malformed float tag"
     (loader-parse-events
     (loader-document (loader-event :scalar :tag "!!float" :value "nope")))
     yaml-kit:yaml-compose-error))
)
(loader-error-cases
  ("rejects a duplicate stream start"
   (loader-compose-all-events
    (list (loader-event :stream-start)
          (loader-event :stream-start)))
   yaml-kit:yaml-compose-error)
  ("rejects document start before stream start"
   (loader-compose-all-events (list (loader-event :document-start)))
   yaml-kit:yaml-compose-error)
  ("rejects document start after stream end"
   (loader-compose-all-events
    (list (loader-event :stream-start)
          (loader-event :stream-end)
          (loader-event :document-start)))
   yaml-kit:yaml-compose-error)
  ("rejects document start inside an open document"
   (loader-compose-all-events
    (list (loader-event :stream-start)
          (loader-event :document-start)
          (loader-event :document-start)))
   yaml-kit:yaml-compose-error)
  ("rejects stream end before document end"
   (loader-compose-all-events
    (list (loader-event :stream-start)
          (loader-event :document-start)
          (loader-event :stream-end)))
   yaml-kit:yaml-compose-error)
  ("rejects document end outside a document"
   (loader-compose-all-events
    (list (loader-event :stream-start)
          (loader-event :document-end)))
   yaml-kit:yaml-compose-error)
  ("rejects document end without a root node"
   (loader-compose-all-events
    (list (loader-event :stream-start)
          (loader-event :document-start)
          (loader-event :document-end)))
   yaml-kit:yaml-compose-error)
  ("rejects a scalar outside a document"
   (loader-compose-all-events
    (list (loader-event :stream-start)
          (loader-event :scalar :value "value")))
   yaml-kit:yaml-compose-error)
  ("rejects a scalar before stream start"
   (loader-compose-all-events (list (loader-event :scalar :value "value")))
   yaml-kit:yaml-compose-error)
  ("rejects an event after stream end"
   (loader-compose-all-events
    (list (loader-event :stream-start)
          (loader-event :stream-end)
          (loader-event :scalar :value "value")))
   yaml-kit:yaml-compose-error)
  ("rejects a mismatched collection end"
   (loader-compose-all-events
    (list (loader-event :stream-start)
          (loader-event :document-start)
          (loader-event :sequence-start)
          (loader-event :mapping-end)))
   yaml-kit:yaml-compose-error))
