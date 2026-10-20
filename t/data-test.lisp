;;;; t/data-test.lisp
(in-package #:cl-yaml-kit/test)

(describe "data contract"
  (it "identifies each YAML sentinel"
    (expect (yaml-kit:yaml-null-p yaml-kit:+yaml-null+)
            :to-be-truthy)
    (expect (yaml-kit:yaml-false-p yaml-kit:+yaml-false+)
            :to-be-truthy))
  (it "distinguishes the YAML sentinels"
    (expect (yaml-kit:yaml-null-p yaml-kit:+yaml-false+)
            :to-equal nil)
    (expect (yaml-kit:yaml-false-p yaml-kit:+yaml-null+)
            :to-equal nil))
  (it "parses null as the YAML null sentinel"
    (expect "null" :to-parse-as yaml-kit:+yaml-null+))
  (it "expands reader resource declarations to one function"
    (let ((expansion
            (macroexpand-1
             '(yaml-kit::define-yaml-resource-function resource/k
                  (input k)
                (:group :reader :keywords ((schema :core)))
                (funcall k input)))))
      (expect expansion :to-equal
              '(defun resource/k
                 (input k &key (schema :core)
                   (yaml-kit::max-input-length
                    yaml-kit::+default-max-input-length+)
                   (yaml-kit::max-depth yaml-kit::+default-max-depth+)
                   (yaml-kit::max-scalar-length
                    yaml-kit::+default-max-scalar-length+)
                   (yaml-kit::max-nodes yaml-kit::+default-max-nodes+)
                   (yaml-kit::max-alias-expansions
                    yaml-kit::+default-max-alias-expansions+))
                 (funcall k input)))))
  (it "rejects duplicate resource keywords during macroexpansion"
    (let ((condition
            (handler-case
                (progn
                  (macroexpand-1
                   '(yaml-kit::define-yaml-resource-function resource/k
                        (input)
                      (:group :input
                       :keywords ((max-input-length 42)))
                      input))
                  nil)
              (error (condition) condition))))
      (expect condition :to-be-truthy)
      (expect (search "Duplicate YAML keyword argument"
                      (princ-to-string condition))
              :to-be-truthy)))
  (it "preserves ordered mapping entries and copies the input"
    (let* ((entries (list (cons "a" 1) (cons "a" 2)))
           (mapping (yaml-kit:make-yaml-mapping entries)))
      (setf (car entries) '("changed" . 9))
      (expect (yaml-kit:yaml-mapping-p mapping) :to-be-truthy)
      (expect (yaml-kit:yaml-mapping-entries mapping) :to-equal
              '(("a" . 1) ("a" . 2)))))
  (it "creates an empty mapping when entries are omitted"
    (let ((mapping (yaml-kit:make-yaml-mapping)))
      (expect (yaml-kit:yaml-mapping-entries mapping) :to-equal nil)))
  (it-each
      ((scalar-style yaml-kit::scalar-style :plain)
       (collection-style yaml-kit::collection-style :flow)
       (chomping-indicator yaml-kit::chomping-indicator :keep)
       (indentation-indicator yaml-kit::indentation-indicator 4))
    "accepts a value of type ~S"
    (name type value)
    (declare (ignore name))
    (expect (typep value type) :to-be-truthy))

  (it "compares YAML mapping entries in order"
    (let ((mapping
            (yaml-kit:make-yaml-mapping
             (list (cons "a" 1) (cons "b" 2)))))
      (expect (%yaml-value-equal-p
               mapping
               (yaml-kit:make-yaml-mapping
                (list (cons "a" 1) (cons "b" 2))))
              :to-be-truthy)
      (expect (%yaml-value-equal-p
               mapping
               (yaml-kit:make-yaml-mapping
                (list (cons "b" 2) (cons "a" 1))))
              :to-be-falsy)))
  (it "recursively compares cons and vector values"
    (let ((actual (cons "root" (vector 1 (cons 2 3))))
          (same (cons "root" (vector 1 (cons 2 3))))
          (different (cons "root" (vector 1 (cons 2 4)))))
      (expect (%yaml-value-equal-p actual same) :to-be-truthy)
      (expect (%yaml-value-equal-p actual different) :to-be-falsy)))
  (it "compares hash tables independent of entry order"
    (let ((actual (make-hash-table :test #'equal))
          (expected (make-hash-table :test #'equal)))
      (setf (gethash "first" actual) 1
            (gethash "second" actual) '(2 3)
            (gethash "second" expected) '(2 3)
            (gethash "first" expected) 1)
      (expect (%yaml-value-equal-p actual expected) :to-be-truthy)
      (setf (gethash "first" expected) 4)
      (expect (%yaml-value-equal-p actual expected) :to-be-falsy)))
  (it "compares cyclic values without recurring forever"
    (let ((actual (list :node nil))
          (expected (list :node nil)))
      (setf (cdr (cdr actual)) actual
            (cdr (cdr expected)) expected)
      (expect (%yaml-value-equal-p actual expected) :to-be-truthy)))
  (it "compares YAML sentinels by identity"
    (expect (%yaml-value-equal-p yaml-kit:+yaml-null+ yaml-kit:+yaml-null+)
            :to-be-truthy)
    (expect (%yaml-value-equal-p (make-symbol "%YAML-NULL")
                                 yaml-kit:+yaml-null+)
            :to-be-falsy)
    (expect (%yaml-value-equal-p yaml-kit:+yaml-null+ yaml-kit:+yaml-false+)
            :to-be-falsy))
  (it "rejects equal-valued floats of different types"
    (expect (%yaml-value-equal-p (coerce 1 'single-float)
                                 (coerce 1 'double-float))
            :to-be-falsy))
  (it "compares NaNs of the same float type"
    (let ((actual (sb-kernel:make-double-float #x7ff80000 0))
          (expected (sb-kernel:make-double-float #x7ff80000 1)))
      (expect (%yaml-value-equal-p actual expected) :to-be-truthy)))
  (it "distinguishes positive and negative zero"
    (let ((positive-zero
            (sb-kernel:make-double-float 0 0))
          (negative-zero
            (sb-kernel:make-double-float -2147483648 0)))
      (expect (%yaml-value-equal-p positive-zero negative-zero)
              :to-be-falsy)))
  (it "matches emitted YAML"
    (expect 42 :to-emit-as #.(format nil "42~%")))
  (it "matches YAML round trips"
    (expect #(1 2) :to-round-trip))
  (it "matches YAML parse errors"
    (expect "\"unterminated"
            :to-signal-yaml-error 'yaml-kit:yaml-parse-error)))
