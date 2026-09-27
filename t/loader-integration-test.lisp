;;;; t/loader-integration-test.lisp
(in-package #:cl-yaml-kit/test)

(describe "loader integration"
  (it "parses all documents from an event source"
    (let ((events (list (loader-event :stream-start)
                        (loader-event :document-start)
                        (loader-event :scalar :value "one")
                        (loader-event :document-end)
                        (loader-event :document-start)
                        (loader-event :scalar :value "two")
                        (loader-event :document-end)
                        (loader-event :stream-end))))
      (expect (loader-parse-all-events events :schema :failsafe)
              :to-equal '("one" "two"))))

  (it "supports first-only composition and empty event sources"
    (let ((events (list (loader-event :stream-start)
                        (loader-event :document-start)
                        (loader-event :scalar :value "first")
                        (loader-event :document-end)
                        (loader-event :document-start)
                        (loader-event :scalar :value "second")
                        (loader-event :document-end)
                        (loader-event :stream-end))))
      (expect (length (loader-compose-all-events events :first-only t))
              :to-equal 1))
    (expect (loader-compose-all-events nil) :to-equal nil))

  (it "delivers completed documents to a document handler"
    (let ((documents nil)
          (events (loader-document (loader-event :scalar :value "handled"))))
      (expect (loader-compose-all-events
               events
               :document-handler (lambda (node)
                                    (push (yaml-kit:scalar-node-value node)
                                          documents)))
              :to-equal nil)
      (expect documents :to-equal '("handled"))))

  (it "rejects an unknown event in the integration path"
    (expect (handler-case
                (progn (loader-compose-all-events
                        (list (loader-event :stream-start) 42))
                       nil)
              (yaml-kit:yaml-compose-error () t))
            :to-be-truthy))

  (it "exposes compose-all and reports resource limits"
    (let ((events (loader-document
                   (loader-event :scalar :value "long"))))
      (expect (length (loader-compose-all-events events)) :to-equal 1)
      (expect (handler-case
                  (progn (loader-compose-all-events events :max-nodes 0) nil)
                (yaml-kit:yaml-resource-limit-error (condition)
                  (and (equal (yaml-kit::yaml-resource-limit-error-limit condition)
                              0)
                       (equal (yaml-kit::yaml-resource-limit-error-actual condition)
                              1))))
              :to-be-truthy)))

  (it "covers the direct resource limit contract"
    (let ((limit (symbol-function 'yaml-kit::%limit!)))
      (expect (funcall limit 0 nil) :to-be-falsy)
      (expect (handler-case
                  (progn (funcall limit 2 1 "nodes") nil)
                (yaml-kit:yaml-resource-limit-error (condition)
                  (and (equal (yaml-kit::yaml-resource-limit-error-limit-name condition)
                              "nodes")
                       (equal (yaml-kit::yaml-resource-limit-error-limit condition) 1)
                       (equal (yaml-kit::yaml-resource-limit-error-actual condition) 2))))
              :to-be-truthy)))

  (it "uses list and alist construction options"
    (let ((events (loader-document
                   (loader-event :sequence-start)
                   (loader-event :scalar :value "a")
                   (loader-event :scalar :value "b")
                   (loader-event :sequence-end))))
      (expect (loader-parse-events events :schema :failsafe :sequence-type :list)
              :to-equal '("a" "b")))
    (let ((events (loader-document
                   (loader-event :mapping-start)
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "value")
                   (loader-event :mapping-end))))
      (expect (loader-parse-events events :schema :failsafe :mapping-type :alist)
              :to-equal '(("key" . "value")))))

  (it "reads reader input through the loader path"
    (with-input-from-string (stream "answer: 42")
      (let ((value (yaml-kit:read-yaml stream)))
        (expect (gethash "answer" value) :to-equal 42))))

  (it "parses reader input with public compose-all and parse-all paths"
    (with-input-from-string (stream (format nil "one~%"))
      (expect (length (yaml-kit:compose-all stream)) :to-equal 1))
    (with-input-from-string (stream (format nil "one~%"))
      (expect (yaml-kit:parse-all stream :schema :failsafe)
              :to-equal '("one"))))

  (it "preserves aliases as shared nodes during composition"
    (let* ((events (loader-document
                    (loader-event :mapping-start)
                    (loader-event :scalar :value "left")
                    (loader-event :sequence-start :anchor "a")
                    (loader-event :scalar :value "x")
                    (loader-event :sequence-end)
                    (loader-event :scalar :value "right")
                    (loader-event :alias :anchor "a")
                    (loader-event :mapping-end)))
           (node (loader-compose-events events)))
      (expect (eq (yaml-kit:sequence-node-items
                   (cdr (first (yaml-kit:mapping-node-pairs node))))
                  (yaml-kit:sequence-node-items
                   (cdr (second (yaml-kit:mapping-node-pairs node)))))
              :to-be-truthy)))

  (it "preserves aliases as shared values during construction"
    (let ((events (loader-document
                   (loader-event :sequence-start)
                   (loader-event :sequence-start :anchor "a")
                   (loader-event :scalar :value "x")
                   (loader-event :sequence-end)
                   (loader-event :alias :anchor "a")
                   (loader-event :sequence-end))))
      (let ((value (loader-parse-events events)))
        (expect (eq (aref value 0) (aref value 1)) :to-be-truthy))))

  (it "applies duplicate-key policies"
    (let ((events (loader-document
                   (loader-event :mapping-start)
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "first")
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "last")
                   (loader-event :mapping-end))))
      (expect (gethash "key" (loader-parse-events events :duplicate-key-policy :first))
              :to-equal "first")
      (expect (gethash "key" (loader-parse-events events :duplicate-key-policy :last))
              :to-equal "last")
      (expect (handler-case (progn (loader-parse-events events) nil)
                (yaml-kit:yaml-compose-error () t))
              :to-be-truthy)))

  (it "applies duplicate policies to alists and hash tables"
    (let ((events (loader-document
                   (loader-event :mapping-start)
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "first")
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "last")
                   (loader-event :mapping-end))))
      (expect (handler-case (progn (loader-parse-events events :mapping-type :alist) nil)
                (yaml-kit:yaml-compose-error () t)) :to-be-truthy)
      (expect (gethash "key" (loader-parse-events events :duplicate-key-policy :first))
              :to-equal "first")
      (expect (gethash "key" (loader-parse-events events :duplicate-key-policy :last))
              :to-equal "last")))

  (it "rejects incompatible collection tags and enforces limits"
    (let ((events (loader-document
                   (loader-event :sequence-start :tag "!!str")
                   (loader-event :sequence-end))))
      (expect (handler-case (progn (loader-parse-events events) nil)
                (yaml-kit:yaml-compose-error () t))
              :to-be-truthy))
    (let ((events (loader-document (loader-event :scalar :value "long"))))
      (expect (handler-case
                  (progn (loader-parse-events events :max-scalar-length 3) nil)
                (yaml-kit:yaml-resource-limit-error () t))
              :to-be-truthy))))
