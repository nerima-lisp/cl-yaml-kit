(in-package #:cl-yaml-kit/test)

(describe "loader"
  (it "allows anchor redefinition and resolves the latest definition"
    (let* ((events (loader-document
                    (loader-event :sequence-start)
                    (loader-event :scalar :anchor "a" :value "first")
                    (loader-event :scalar :anchor "a" :value "second")
                    (loader-event :alias :anchor "a")
                    (loader-event :sequence-end)))
           (node (loader-compose-events events)))
      (expect (eq (second (yaml-kit:sequence-node-items node))
                  (third (yaml-kit:sequence-node-items node)))
              :to-be-truthy)))

  (it "rejects a stream whose document end is omitted"
    (expect (handler-case
                (progn
                  (loader-compose-all-events
                   (list (loader-event :stream-start)
                         (loader-event :document-start)
                         (loader-event :scalar :value "value")))
                  nil)
              (yaml-kit:yaml-compose-error () t))
            :to-be-truthy))
  (loader-error-cases
    ("rejects a second root after a completed collection"
     (loader-compose-events
      (loader-document
       (loader-event :scalar :value "one")
       (loader-event :sequence-start)
       (loader-event :sequence-end)))
     yaml-kit:yaml-compose-error)
    ("rejects a mapping with a scalar collection tag"
     (loader-parse-events
      (loader-document
       (loader-event :mapping-start :tag "!!str")
       (loader-event :mapping-end)))
     yaml-kit:yaml-compose-error)
    ("rejects an unclosed collection"
     (loader-compose-events
      (list (loader-event :stream-start)
            (loader-event :document-start)
            (loader-event :sequence-start)))
     yaml-kit:yaml-compose-error)
    ("rejects document end inside collection"
     (loader-compose-events
      (list (loader-event :stream-start)
            (loader-event :document-start)
            (loader-event :sequence-start)
            (loader-event :document-end)))
     yaml-kit:yaml-compose-error)
    ("rejects an unknown event"
     (loader-compose-events (list (loader-event :stream-start) 42))
     yaml-kit:yaml-compose-error)
    ("rejects an unknown schema"
     (yaml-kit::%schema-table :unknown)
     simple-error)
    ("rejects a non-node"
     (yaml-kit::construct 42)
     yaml-kit:yaml-compose-error))

)
