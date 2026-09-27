;;;; t/data-test.lisp
(in-package #:cl-yaml-kit/test)

(defmacro define-type-contract-test (name value)
  `(it ,(format nil "accepts ~A" name)
     (expect (typep ,value ',name) :to-be-truthy)))

(describe "data contract"
  (it "provides distinct YAML sentinels"
    (expect (yaml-kit:yaml-null-p yaml-kit:+yaml-null+) :to-be-truthy)
    (expect (yaml-kit:yaml-false-p yaml-kit:+yaml-false+) :to-be-truthy)
    (expect (yaml-kit:yaml-null-p yaml-kit:+yaml-false+) :to-equal nil)
    (expect (yaml-kit:yaml-false-p yaml-kit:+yaml-null+) :to-equal nil)
    (expect (eq yaml-kit:+yaml-null+ (yaml-kit:parse "null")) :to-be-truthy))
  (it "preserves ordered mapping entries and copies the input"
    (let* ((entries (list (cons "a" 1) (cons "a" 2)))
           (mapping (yaml-kit:make-yaml-mapping entries)))
      (setf (car entries) '("changed" . 9))
      (expect (yaml-kit:yaml-mapping-p mapping) :to-be-truthy)
      (expect (yaml-kit:yaml-mapping-entries mapping) :to-equal
              '(("a" . 1) ("a" . 2)))))
  (define-type-contract-test yaml-kit::scalar-style :plain)
  (define-type-contract-test yaml-kit::collection-style :flow)
  (define-type-contract-test yaml-kit::chomping-indicator :keep)
  (define-type-contract-test yaml-kit::indentation-indicator 4))
