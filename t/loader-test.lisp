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

  (it-each
      ((comment-spans :comment-spans "# comment")
       (compose :compose "value")
       (compose-all :compose-all "value")
       (parse :parse "value")
       (parse-all :parse-all "value")
       (read-yaml :read-yaml "value"))
    "returns primary callback value once for ~S"
    (name operation input)
    (declare (ignore name))
    (let* ((calls 0)
           (callback (lambda (value)
                       (declare (ignore value))
                       (incf calls)
                       (values :primary :secondary)))
           (returned-values
             (multiple-value-list
              (ecase operation
                (:comment-spans (yaml-kit:comment-spans/k input callback))
                (:compose (yaml-kit:compose/k input callback))
                (:compose-all (yaml-kit:compose-all/k input callback))
                (:parse (yaml-kit:parse/k input callback))
                (:parse-all (yaml-kit:parse-all/k input callback))
                (:read-yaml
                 (with-input-from-string (stream input)
                   (yaml-kit:read-yaml/k stream callback)))))))
      (expect calls :to-equal 1)
      (expect returned-values :to-equal (list :primary))))
  (cl-weave:it-each
      ((comment-spans :comment-spans)
       (compose :compose)
       (compose-all :compose-all)
       (parse :parse)
       (parse-all :parse-all)
       (read-yaml :read-yaml))
    "does not call its callback on an input limit error for ~S"
    (name operation)
    (declare (ignore name))
    (let* ((calls 0)
           (callback (lambda (value)
                       (declare (ignore value))
                       (incf calls))))
      (expect
       (lambda ()
         (ecase operation
           (:comment-spans
            (yaml-kit:comment-spans/k "x" callback :max-input-length 0))
           (:compose
            (yaml-kit:compose/k "x" callback :max-input-length 0))
           (:compose-all
            (yaml-kit:compose-all/k "x" callback :max-input-length 0))
           (:parse
            (yaml-kit:parse/k "x" callback :max-input-length 0))
           (:parse-all
            (yaml-kit:parse-all/k "x" callback :max-input-length 0))
           (:read-yaml
            (with-input-from-string (stream "x")
              (yaml-kit:read-yaml/k stream callback :max-input-length 0)))))
       :to-signal-loader-resource-limit
       (list :name "input length" :limit 0 :actual 1))
      (expect calls :to-equal 0)))

  (cl-weave:it-each
      ((missing-document-end
         :compose-all-events
         ((:stream-start) (:document-start) (:scalar :value "value"))
         ()
         yaml-kit:yaml-compose-error)
       (second-root
         :compose-events
         ((:stream-start) (:document-start) (:scalar :value "one")
          (:sequence-start) (:sequence-end) (:document-end) (:stream-end))
         ()
         yaml-kit:yaml-compose-error)
       (scalar-collection-tag
         :parse-events
         ((:stream-start) (:document-start) (:mapping-start :tag "!!str")
          (:mapping-end) (:document-end) (:stream-end))
         ()
         yaml-kit:yaml-compose-error)
       (unclosed-collection
         :compose-events
         ((:stream-start) (:document-start) (:sequence-start))
         ()
         yaml-kit:yaml-compose-error)
       (document-end-inside-collection
         :compose-events
         ((:stream-start) (:document-start) (:sequence-start) (:document-end))
         ()
         yaml-kit:yaml-compose-error)
       (unknown-event
         :compose-events
         ((:stream-start) 42)
         ()
         yaml-kit:yaml-compose-error)
       (unknown-schema :schema-table :unknown () simple-error)
       (non-node :construct 42 () yaml-kit:yaml-compose-error))
    "rejects loader contract case ~S"
    (name operation input options expected)
    (expect (lambda () (loader-case-call operation input options))
            :to-signal-loader-error
            expected)))
