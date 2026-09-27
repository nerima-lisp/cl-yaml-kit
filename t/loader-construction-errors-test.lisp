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
