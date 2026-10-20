;;;; t/loader-construction-test.lisp
(in-package #:cl-yaml-kit/test)

(describe "loader construction"
  (cl-weave:it-each
      ((empty-mapping :parse-events
         ((:stream-start) (:document-start) (:mapping-start) (:mapping-end)
          (:document-end) (:stream-end))
         (:mapping-type :yaml-mapping)
         :empty-mapping)
       (positive-infinity :parse-document
         ((:scalar :value ".INF"))
         ()
         :positive-infinity)
       (negative-infinity :parse-document
         ((:scalar :value "-.INF"))
         ()
         :negative-infinity)
       (nan :parse-document
         ((:scalar :value ".NaN"))
         ()
         :nan))
    "constructs ~S"
    (name operation input options expected)
    (declare (ignore name))
    (let ((value (loader-case-call operation input options)))
      (expect
       (case expected
         (:empty-mapping
          (and (yaml-kit:yaml-mapping-p value)
               (null (yaml-kit:yaml-mapping-entries value))))
         (:positive-infinity
          (and (floatp value)
               (yaml-kit::%float-infinity-p value)
               (not (minusp value))))
         (:negative-infinity
          (and (floatp value)
               (yaml-kit::%float-infinity-p value)
               (minusp value)))
         (:nan (and (floatp value) (yaml-kit::%float-nan-p value))))
       :to-be-truthy)))

  (it "constructs a non-empty YAML mapping"
    (let ((value (loader-parse-events
                  (loader-document
                   (loader-event :mapping-start)
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "value")
                   (loader-event :mapping-end))
                  :mapping-type :yaml-mapping)))
      (expect (yaml-kit:yaml-mapping-entries value)
              :to-equal
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

  (it "constructs and caches direct list sequences"
    (let* ((node (yaml-kit:make-sequence-node
                  :items (list (yaml-kit:make-scalar-node :value "a" :style :plain)
                               (yaml-kit:make-scalar-node :value "b" :style :plain))))
           (memo (make-hash-table :test #'eq))
           (construct-sequence
             (symbol-function 'yaml-kit::%construct-sequence))
           (value (funcall construct-sequence node :list memo #'identity))
           (cache-hit (funcall construct-sequence node :list memo #'identity)))
      (expect value :to-equal (yaml-kit:sequence-node-items node))
      (expect (eq cache-hit value) :to-be-truthy)))

  (it "constructs and caches direct vector sequences"
    (let* ((node (yaml-kit:make-sequence-node
                  :items (list (yaml-kit:make-scalar-node :value "a" :style :plain)
                               (yaml-kit:make-scalar-node :value "b" :style :plain))))
           (memo (make-hash-table :test #'eq))
           (construct-sequence
             (symbol-function 'yaml-kit::%construct-sequence))
           (value (funcall construct-sequence node :vector memo #'identity)))
      (expect (coerce value 'list)
              :to-equal
              (yaml-kit:sequence-node-items node))
      (expect (eq (gethash node memo) value) :to-be-truthy)))

  (it "preserves empty list and vector sequences"
    (let* ((node (yaml-kit:make-sequence-node))
           (construct-sequence
             (symbol-function 'yaml-kit::%construct-sequence))
           (list-value (funcall construct-sequence
                                node :list (make-hash-table :test #'eq)
                                #'identity))
           (vector-value (funcall construct-sequence
                                  node :vector (make-hash-table :test #'eq)
                                  #'identity)))
      (expect list-value :to-be-falsy)
      (expect (length vector-value) :to-equal 0)))

  (it "accepts collection tags by node kind"
    (let ((compatible
            (symbol-function 'yaml-kit::%collection-tag-compatible-p)))
      (expect (funcall compatible
                       (yaml-kit:make-mapping-node)
                       "tag:yaml.org,2002:map")
              :to-be-truthy)
      (expect (every (lambda (tag)
                       (funcall compatible
                                (yaml-kit:make-mapping-node :tag tag)
                                "tag:yaml.org,2002:map"))
                     '("!" "?" "!!map"))
              :to-be-truthy)
      (expect (notany (lambda (tag)
                        (funcall compatible
                                 (yaml-kit:make-mapping-node :tag tag)
                                 "tag:yaml.org,2002:map"))
                      '("!!str" "!!seq"))
              :to-be-truthy)))

  (it "constructs mapping values through each collection contract"
    (let* ((key (yaml-kit:make-scalar-node :value "key" :style :plain))
           (value (yaml-kit:make-scalar-node :value "value" :style :plain))
           (node (yaml-kit:make-mapping-node :pairs (list (cons key value))))
           (mapping (symbol-function 'yaml-kit::%construct-mapping))
           (walk (lambda (object)
                   (if (yaml-kit:scalar-node-p object)
                       (yaml-kit:scalar-node-value object)
                       object))))
      (expect (yaml-kit:yaml-mapping-p
               (funcall mapping node :yaml-mapping :error
                        (make-hash-table :test #'eq) walk))
              :to-be-truthy)
      (expect (funcall mapping node :alist :error
                       (make-hash-table :test #'eq) walk)
              :to-equal '(("key" . "value")))
      (let ((table (funcall mapping node :hash-table :error
                            (make-hash-table :test #'equal) walk)))
        (expect (gethash "key" table) :to-equal "value"))))

  (it "applies first and last duplicate policies to hash tables"
    (let* ((key (yaml-kit:make-scalar-node :value "key" :style :plain))
           (first-value (yaml-kit:make-scalar-node :value "first" :style :plain))
           (last-value (yaml-kit:make-scalar-node :value "last" :style :plain))
           (node (yaml-kit:make-mapping-node
                  :pairs (list (cons key first-value) (cons key last-value))))
           (mapping (symbol-function 'yaml-kit::%construct-mapping))
           (walk (lambda (object)
                   (if (yaml-kit:scalar-node-p object)
                       (yaml-kit:scalar-node-value object)
                       object))))
      (expect (gethash "key"
                       (funcall mapping node :hash-table :first
                                (make-hash-table :test #'equal) walk))
              :to-equal "first")
      (expect (gethash "key"
                       (funcall mapping node :hash-table :last
                                (make-hash-table :test #'equal) walk))
              :to-equal "last")))

  (it "rejects duplicate hash keys under the error policy"
    (let* ((key (yaml-kit:make-scalar-node :value "key" :style :plain))
           (node (yaml-kit:make-mapping-node
                  :pairs (list (cons key key) (cons key key))))
           (mapping (symbol-function 'yaml-kit::%construct-mapping))
           (walk (lambda (object)
                   (if (yaml-kit:scalar-node-p object)
                       (yaml-kit:scalar-node-value object)
                       object))))
      (expect
       (lambda ()
         (funcall mapping node :hash-table :error
                  (make-hash-table :test #'equal) walk))
       :to-signal-loader-error
       'yaml-kit:yaml-compose-error)))

  (it "rejects a non-scalar direct hash key"
    (let* ((node (yaml-kit:make-mapping-node
                  :pairs (list (cons (yaml-kit:make-yaml-mapping)
                                     (yaml-kit:make-scalar-node :value "value")))))
           (mapping (symbol-function 'yaml-kit::%construct-mapping)))
      (expect
       (lambda ()
         (funcall mapping node :hash-table :error
                  (make-hash-table :test #'equal) #'identity))
       :to-signal-loader-error
       'yaml-kit:yaml-compose-error)))

  (it "accepts an unknown application collection tag"
    (expect (funcall (symbol-function 'yaml-kit::%collection-tag-compatible-p)
                     (yaml-kit:make-mapping-node :tag "!app/map")
                     "tag:yaml.org,2002:map")
            :to-be-truthy)
    (expect (funcall (symbol-function 'yaml-kit::%collection-tag-compatible-p)
                     (yaml-kit:make-mapping-node :tag "!app")
                     "tag:yaml.org,2002:map")
            :to-be-truthy))

  (cl-weave:it-each
      ((sequence-primary :sequence "tag:yaml.org,2002:seq")
       (sequence-shorthand :sequence "!!seq")
       (sequence-non-specific :sequence "!")
       (mapping-primary :mapping "tag:yaml.org,2002:map")
       (mapping-shorthand :mapping "!!map")
       (mapping-application :mapping "!app/map"))
    "constructs empty ~S with tag ~S"
    (name kind tag)
    (declare (ignore name))
    (let ((value (loader-parse-events
                  (loader-empty-collection-events kind tag))))
      (expect (case kind
                (:sequence (and (vectorp value) (zerop (length value))))
                (:mapping (and (hash-table-p value)
                               (zerop (hash-table-count value)))))
              :to-be-truthy))))
