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
  (define-type-contract-test yaml-kit::scalar-style :plain)
  (define-type-contract-test yaml-kit::collection-style :flow)
  (define-type-contract-test yaml-kit::chomping-indicator :keep)
  (define-type-contract-test yaml-kit::indentation-indicator 4))
