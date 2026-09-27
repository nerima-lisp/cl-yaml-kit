;;;; t/loader-construction-test.lisp
(in-package #:cl-yaml-kit/test)

(describe "loader construction"
  (loader-predicate-cases
    ("empty mapping can be a YAML mapping"
     (loader-parse-events
      (loader-empty-collection-events :mapping nil)
      :mapping-type :yaml-mapping)
     (lambda (value)
       (and (yaml-kit:yaml-mapping-p value)
            (null (yaml-kit:yaml-mapping-entries value)))))
    ("positive infinity is a double float"
     (loader-parse-events
      (loader-document (loader-event :scalar :value ".INF")))
     (lambda (value)
       (and (floatp value) (> value most-positive-double-float))))
    ("negative infinity is a double float"
     (loader-parse-events
      (loader-document (loader-event :scalar :value "-.INF")))
     (lambda (value)
       (and (floatp value) (< value (- most-positive-double-float))))))

  (it "constructs a non-empty YAML mapping"
    (let ((value (loader-parse-events
                  (loader-document
                   (loader-event :mapping-start)
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "value")
                   (loader-event :mapping-end))
                  :mapping-type :yaml-mapping)))
      (expect (yaml-kit:yaml-mapping-entries value) :to-equal
              '(("key" . "value")))))

  (it "constructs alists with first and last duplicate policies"
    (let ((events (loader-document
                   (loader-event :mapping-start)
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "first")
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "last")
                   (loader-event :mapping-end))))
      (expect (loader-parse-events events :mapping-type :alist
                                   :duplicate-key-policy :first)
              :to-equal '(("key" . "first")))
      (expect (loader-parse-events events :mapping-type :alist
                                   :duplicate-key-policy :last)
              :to-equal '(("key" . "last")))))

  (loader-collection-cases
    ("sequence accepts primary tag" :sequence "tag:yaml.org,2002:seq"
     (lambda (value) (and (vectorp value) (= (length value) 0))))
    ("sequence accepts shorthand tag" :sequence "!!seq"
     (lambda (value) (and (vectorp value) (= (length value) 0))))
    ("sequence accepts non-specific tags" :sequence "!"
     (lambda (value) (and (vectorp value) (= (length value) 0))))
    ("mapping accepts primary tag" :mapping "tag:yaml.org,2002:map"
     (lambda (value) (and (hash-table-p value) (= (hash-table-count value) 0))))
    ("mapping accepts shorthand tag" :mapping "!!map"
     (lambda (value) (and (hash-table-p value) (= (hash-table-count value) 0))))
    ("mapping accepts application tag" :mapping "!app/map"
     (lambda (value) (and (hash-table-p value) (= (hash-table-count value) 0)))))

  (loader-error-cases
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
     yaml-kit:yaml-compose-error)))
