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
       (and (floatp value) (< value (- most-positive-double-float)))))
    ("NaN is a double float"
     (loader-parse-events
      (loader-document (loader-event :scalar :value ".NaN")))
     (lambda (value) (and (floatp value) (not (= value value))))))

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

  (it "constructs sequences directly through the sequence contract"
    (let* ((node (yaml-kit:make-sequence-node
                  :items (list (yaml-kit:make-scalar-node :value "a" :style :plain)
                               (yaml-kit:make-scalar-node :value "b" :style :plain))))
           (empty-node (yaml-kit:make-sequence-node))
           (list-memo (make-hash-table :test #'eq))
           (vector-memo (make-hash-table :test #'eq))
           (list-value (funcall (symbol-function 'yaml-kit::%construct-sequence)
                                node :list list-memo #'identity))
           (list-cache-hit (funcall (symbol-function 'yaml-kit::%construct-sequence)
                                    node :list list-memo #'identity))
           (vector-value (funcall (symbol-function 'yaml-kit::%construct-sequence)
                                  node :vector vector-memo #'identity)))
      (expect list-value :to-equal (yaml-kit:sequence-node-items node))
      (expect (eq list-cache-hit list-value) :to-be-truthy)
      (expect (coerce vector-value 'list) :to-equal
              (yaml-kit:sequence-node-items node))
      (expect (eq (gethash node vector-memo) vector-value) :to-be-truthy)
      (expect (funcall (symbol-function 'yaml-kit::%construct-sequence)
                       empty-node :list (make-hash-table :test #'eq) #'identity)
              :to-be-falsy)
      (expect (length (funcall (symbol-function 'yaml-kit::%construct-sequence)
                               empty-node :vector (make-hash-table :test #'eq) #'identity))
              :to-equal 0)))

  (it "calls collection construction contracts directly"
    (let* ((key (yaml-kit:make-scalar-node :value "key" :style :plain))
           (value (yaml-kit:make-scalar-node :value "value" :style :plain))
           (node (yaml-kit:make-mapping-node :pairs (list (cons key value))))
           (memo (make-hash-table :test #'eq))
           (compatible (symbol-function 'yaml-kit::%collection-tag-compatible-p))
           (mapping (symbol-function 'yaml-kit::%construct-mapping))
           (walk (lambda (object)
                   (if (yaml-kit:scalar-node-p object)
                       (yaml-kit:scalar-node-value object)
                       object))))
      (expect (funcall compatible node "tag:yaml.org,2002:map") :to-be-truthy)
      (dolist (tag '("!" "?" "!!map"))
        (expect (funcall compatible
                         (yaml-kit:make-mapping-node :tag tag)
                         "tag:yaml.org,2002:map")
                :to-be-truthy))
      (expect (funcall compatible
                       (yaml-kit:make-mapping-node :tag "!!str")
                       "tag:yaml.org,2002:map")
              :to-be-falsy)
      (expect (funcall compatible
                       (yaml-kit:make-mapping-node :tag "!!seq")
                       "tag:yaml.org,2002:map")
              :to-be-falsy)
      (expect (yaml-kit:yaml-mapping-p
               (funcall mapping node :yaml-mapping :error memo walk))
              :to-be-truthy)
      (expect (funcall mapping node :alist :error
                       (make-hash-table :test #'eq) walk)
              :to-equal '(("key" . "value")))
      (let ((table (funcall mapping node :hash-table :error
                            (make-hash-table :test #'equal) walk)))
        (expect (gethash "key" table) :to-equal "value"))))

  (it "covers direct hash mapping policies and invalid keys"
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
              :to-equal "last")
      (expect (handler-case
                  (progn (funcall mapping node :hash-table :error
                                  (make-hash-table :test #'equal) walk)
                         nil)
                (yaml-kit:yaml-compose-error () t))
              :to-be-truthy)
      (let ((bad-node (yaml-kit:make-mapping-node
                       :pairs (list (cons (yaml-kit:make-yaml-mapping)
                                          last-value)))))
        (expect (handler-case
                    (progn (funcall mapping bad-node :hash-table :error
                                    (make-hash-table :test #'equal) walk)
                           nil)
                  (yaml-kit:yaml-compose-error () t))
                :to-be-truthy))))

  (it "covers scalar conversion error contracts directly"
    (let ((construct-scalar (symbol-function 'yaml-kit::%construct-scalar))
          (node (yaml-kit:make-scalar-node :value ".NaN" :style :plain)))
      (expect (floatp (funcall construct-scalar node :core)) :to-be-truthy)))

  (it "keeps an explicitly non-specific NaN scalar numeric"
    (let ((construct-scalar (symbol-function 'yaml-kit::%construct-scalar))
          (node (yaml-kit:make-scalar-node :value ".NaN" :tag "?"
                                           :style :plain)))
      (expect (floatp (funcall construct-scalar node :core)) :to-be-truthy)))

  (it "accepts an unknown application collection tag"
    (expect (funcall (symbol-function 'yaml-kit::%collection-tag-compatible-p)
                     (yaml-kit:make-mapping-node :tag "!app/map")
                     "tag:yaml.org,2002:map")
            :to-be-truthy)
    (expect (funcall (symbol-function 'yaml-kit::%collection-tag-compatible-p)
                     (yaml-kit:make-mapping-node :tag "!app")
                     "tag:yaml.org,2002:map")
            :to-be-truthy))

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
     yaml-kit:yaml-compose-error)))
