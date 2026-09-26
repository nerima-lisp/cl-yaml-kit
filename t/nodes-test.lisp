;;;; t/nodes-test.lisp
(in-package #:cl-yaml-kit/test)
(describe "node contract"
  (it "constructs a scalar node"
    (let ((node (make-scalar-node :tag "tag:yaml.org,2002:str" :value "hello")))
      (expect (scalar-node-p node) :to-be-truthy)
      (expect (scalar-node-value node) :to-equal "hello"))))
