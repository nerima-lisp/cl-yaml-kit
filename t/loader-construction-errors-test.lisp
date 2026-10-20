(in-package #:cl-yaml-kit/test)

(describe "loader construction errors"
  (cl-weave:it-each
      ((scalar-collection-tag :parse-document
         ((:scalar :tag "!!seq" :value "value"))
         ()
         yaml-kit:yaml-compose-error)
       (malformed-float :parse-document
         ((:scalar :tag "!!float" :value "1 2"))
         ()
         yaml-kit:yaml-compose-error)
       (unknown-alias :compose-document
         ((:alias :anchor "missing"))
         ()
         yaml-kit:yaml-compose-error)
       (two-root-nodes :compose-document
         ((:scalar :value "one") (:scalar :value "two"))
         ()
         yaml-kit:yaml-compose-error)
       (unmatched-collection-end :compose-document
         ((:sequence-end))
         ()
         yaml-kit:yaml-compose-error)
       (odd-mapping :compose-document
         ((:mapping-start) (:scalar :value "key") (:mapping-end))
         ()
         yaml-kit:yaml-compose-error)
       (invalid-mapping-type :parse-document
         ((:scalar :value "x"))
         (:mapping-type :invalid)
         yaml-kit:yaml-compose-error)
       (invalid-sequence-type :parse-document
         ((:scalar :value "x"))
         (:sequence-type :invalid)
         yaml-kit:yaml-compose-error)
       (invalid-duplicate-policy :parse-document
         ((:scalar :value "x"))
         (:duplicate-key-policy :invalid)
         yaml-kit:yaml-compose-error)
       (malformed-integer :parse-document
         ((:scalar :tag "!!int" :value "nope"))
         ()
         yaml-kit:yaml-compose-error)
       (empty-integer :parse-document
         ((:scalar :tag "!!int" :value ""))
         ()
         yaml-kit:yaml-compose-error)
       (malformed-boolean :parse-document
         ((:scalar :tag "!!bool" :value "maybe"))
         ()
         yaml-kit:yaml-compose-error)
       (malformed-null :parse-document
         ((:scalar :tag "!!null" :value "nope"))
         ()
         yaml-kit:yaml-compose-error)
       (non-scalar-hash-key :parse-document
         ((:mapping-start) (:sequence-start) (:sequence-end)
          (:scalar :value "value") (:mapping-end))
         ()
         yaml-kit:yaml-compose-error)
       (malformed-float-value :parse-document
         ((:scalar :tag "!!float" :value "nope"))
         ()
         yaml-kit:yaml-compose-error)
       (duplicate-stream-start :compose-all-events
         ((:stream-start) (:stream-start))
         ()
         yaml-kit:yaml-compose-error)
       (document-before-stream :compose-all-events
         ((:document-start))
         ()
         yaml-kit:yaml-compose-error)
       (document-after-stream-end :compose-all-events
         ((:stream-start) (:stream-end) (:document-start))
         ()
         yaml-kit:yaml-compose-error)
       (document-inside-document :compose-all-events
         ((:stream-start) (:document-start) (:document-start))
         ()
         yaml-kit:yaml-compose-error)
       (stream-end-before-document-end :compose-all-events
         ((:stream-start) (:document-start) (:stream-end))
         ()
         yaml-kit:yaml-compose-error)
       (document-end-outside-document :compose-all-events
         ((:stream-start) (:document-end))
         ()
         yaml-kit:yaml-compose-error)
       (document-end-without-root :compose-all-events
         ((:stream-start) (:document-start) (:document-end))
         ()
         yaml-kit:yaml-compose-error)
       (scalar-outside-document :compose-all-events
         ((:stream-start) (:scalar :value "value"))
         ()
         yaml-kit:yaml-compose-error)
       (scalar-before-stream-start :compose-all-events
         ((:scalar :value "value"))
         ()
         yaml-kit:yaml-compose-error)
       (event-after-stream-end :compose-all-events
         ((:stream-start) (:stream-end) (:scalar :value "value"))
         ()
         yaml-kit:yaml-compose-error)
       (mismatched-collection-end :compose-all-events
         ((:stream-start) (:document-start) (:sequence-start) (:mapping-end))
         ()
         yaml-kit:yaml-compose-error))
    "rejects loader construction case ~S"
    (name operation input options expected)
    (declare (ignore name))
    (expect (lambda () (loader-case-call operation input options))
            :to-signal-loader-error
            expected))

  (cl-weave:it-each
      ((alist :alist)
       (hash-table :hash-table))
    "preserves duplicate mapping cause for ~S"
    (name mapping-type)
    (declare (ignore name))
    (expect
     (lambda ()
       (loader-case-call
        :parse-document
        '((:mapping-start)
          (:scalar :value "key")
          (:scalar :value "first")
          (:scalar :value "key")
          (:scalar :value "last")
          (:mapping-end))
        (list :mapping-type mapping-type :duplicate-key-policy :error)))
     :to-signal-loader-compose-cause
     "duplicate mapping key"))

  (cl-weave:it-fuzz "signals a YAML compose error for generated unknown schemas"
    ((schema (cl-weave:gen-string :min-length 1 :max-length 16
                                  :alphabet "abcXYZ012")))
    (:trials 100 :timeout-per-trial 5)
    (expect (lambda () (yaml-kit:parse "value" :schema schema))
            :to-signal-loader-error
            'yaml-kit:yaml-compose-error)))
