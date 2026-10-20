;;;; t/nodes-test.lisp
(in-package #:cl-yaml-kit/test)
(defparameter *invalid-node-slot-value* -1)

(it "expands define-node into a structure definition"
  (let ((expansion (macroexpand-1
                    '(yaml-kit::define-node probe-node-contract () "probe"))))
    (expect (consp expansion) :to-be-truthy)
    (expect (member (car expansion) '(progn yaml-kit::define-yaml-subtype)) :to-be-truthy))
  (let* ((name (eval '(yaml-kit::define-node probe-node-contract-eval ((value nil)) "probe")))
         (find-function
           (lambda (format-control)
             (let ((name (format nil format-control name)))
               (block found
                 (do-all-symbols (symbol)
                   (when (and (string= (symbol-name symbol) name)
                              (fboundp symbol))
                     (return-from found symbol)))))))
         (constructor (funcall find-function "MAKE-~A"))
         (predicate (funcall find-function "~A-P"))
         (accessor (funcall find-function "~A-VALUE"))
         (value (funcall (symbol-function constructor) :value 42)))
    (expect (funcall (symbol-function predicate) value) :to-be-truthy)
    (expect (funcall (symbol-function accessor) value) :to-equal 42)))
(defmacro define-node-contract-tests (name constructor predicate accessors arguments)
  (let ((keys (mapcar (lambda (accessor)
                        (loop for key in arguments by #'cddr
                              when (and (keywordp key)
                                        (search (string-upcase (symbol-name key))
                                                (string-upcase (symbol-name accessor))
                                                :from-end t))
                                do (return key)))
                      accessors)))
    `(it ,(format nil "constructs ~A" name)
       (let ((value (,constructor ,@arguments)))
         (expect (,predicate value) :to-be-truthy)
         (expect (typep value ',name) :to-be-truthy)
         ,@(mapcar (lambda (accessor key)
                     `(expect (contract-value-equal-p (,accessor value)
                                     (getf (list ,@arguments) ,key))
                              :to-be-truthy))
                   accessors keys)))))

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

(it "uses node constructor defaults"
  (let ((scalar (yaml-kit:make-scalar-node))
        (sequence (yaml-kit:make-sequence-node))
        (mapping (yaml-kit:make-mapping-node)))
    (expect (list (yaml-kit:node-tag scalar)
                  (yaml-kit:node-anchor scalar)
                  (yaml-kit:node-style scalar)
                  (yaml-kit:node-start-mark scalar)
                  (yaml-kit:node-end-mark scalar)
                  (yaml-kit:scalar-node-value scalar))
            :to-equal '(nil nil nil nil nil ""))
    (expect (list (yaml-kit:node-tag sequence)
                  (yaml-kit:node-anchor sequence)
                  (yaml-kit:node-style sequence)
                  (yaml-kit:node-start-mark sequence)
                  (yaml-kit:node-end-mark sequence)
                  (yaml-kit:sequence-node-items sequence))
            :to-equal '(nil nil nil nil nil nil))
    (expect (list (yaml-kit:node-tag mapping)
                  (yaml-kit:node-anchor mapping)
                  (yaml-kit:node-style mapping)
                  (yaml-kit:node-start-mark mapping)
                  (yaml-kit:node-end-mark mapping)
                  (yaml-kit:mapping-node-pairs mapping))
            :to-equal '(nil nil nil nil nil nil))))
(it "rejects invalid node metadata slot values"
  (expect (every (lambda (thunk)
                   (handler-case (funcall thunk)
                     (type-error () t)))
                 (list (lambda () (yaml-kit:make-scalar-node
                                  :tag *invalid-node-slot-value*))
                       (lambda () (yaml-kit:make-scalar-node
                                  :anchor *invalid-node-slot-value*))
                       (lambda () (yaml-kit:make-scalar-node
                                  :style *invalid-node-slot-value*))
                       (lambda () (yaml-kit:make-scalar-node
                                  :start-mark *invalid-node-slot-value*))
                       (lambda () (yaml-kit:make-scalar-node
                                  :end-mark *invalid-node-slot-value*))))
          :to-be-truthy))
