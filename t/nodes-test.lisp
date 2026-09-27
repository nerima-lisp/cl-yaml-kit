;;;; t/nodes-test.lisp
(in-package #:cl-yaml-kit/test)

(it "expands define-node into a structure definition"
  (let ((expansion (macroexpand-1
                    '(yaml-kit::define-node probe-node-contract () "probe"))))
    (expect (consp expansion) :to-be-truthy)
    (expect (equal (car expansion) 'progn) :to-be-truthy)))
(defmacro define-node-contract-tests (name constructor predicate accessors arguments)
  `(it ,(format nil "constructs ~A" name)
     (let ((value (,constructor ,@arguments)))
       (expect (,predicate value) :to-be-truthy)
       (expect (typep value ',name) :to-be-truthy)
       ,@(mapcar (lambda (accessor)
                   `(expect (,accessor value) :to-be-truthy))
                 accessors))))

(define-node-contract-tests yaml-kit:scalar-node yaml-kit:make-scalar-node yaml-kit:scalar-node-p
  (yaml-kit:node-tag yaml-kit:node-anchor yaml-kit:node-style
   yaml-kit:node-start-mark yaml-kit:node-end-mark yaml-kit:scalar-node-value)
  (:tag "tag" :anchor "anchor" :style :plain
   :start-mark (yaml-kit:make-mark 1 2 3) :end-mark (yaml-kit:make-mark 1 4 5)
   :value "value"))
(define-node-contract-tests yaml-kit:sequence-node yaml-kit:make-sequence-node yaml-kit:sequence-node-p
  (yaml-kit:node-tag yaml-kit:node-anchor yaml-kit:node-style
   yaml-kit:node-start-mark yaml-kit:node-end-mark yaml-kit:sequence-node-items)
  (:tag "tag" :anchor "anchor" :style :block
   :start-mark (yaml-kit:make-mark 2 2 6) :end-mark (yaml-kit:make-mark 2 4 8)
   :items '(one two)))
(define-node-contract-tests yaml-kit:mapping-node yaml-kit:make-mapping-node yaml-kit:mapping-node-p
  (yaml-kit:node-tag yaml-kit:node-anchor yaml-kit:node-style
   yaml-kit:node-start-mark yaml-kit:node-end-mark yaml-kit:mapping-node-pairs)
  (:tag "tag" :anchor "anchor" :style :flow
   :start-mark (yaml-kit:make-mark 3 2 9) :end-mark (yaml-kit:make-mark 3 4 11)
   :pairs '((one . two))))
