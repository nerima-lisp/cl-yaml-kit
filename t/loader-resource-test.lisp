;;;; t/loader-resource-test.lisp
(in-package #:cl-yaml-kit/test)

(describe "loader resources"
  (loader-limit-cases
    ("enforces maximum depth"
     (loader-compose-events
      (loader-document
       (loader-event :sequence-start)
       (loader-event :sequence-end))
      :max-depth 0)
     "depth")
    ("enforces maximum nodes"
     (loader-compose-events
      (loader-document (loader-event :scalar :value "x"))
      :max-nodes 0)
     "nodes")
    ("enforces alias expansion limits"
     (loader-compose-events
      (loader-document
       (loader-event :sequence-start :anchor "a")
       (loader-event :sequence-end)
       (loader-event :alias :anchor "a"))
      :max-alias-expansions 0)
     "alias expansions")
    ("enforces reader input limits"
     (with-input-from-string (stream "x")
       (yaml-kit:parse stream :max-input-length 0))
     "input length")))
