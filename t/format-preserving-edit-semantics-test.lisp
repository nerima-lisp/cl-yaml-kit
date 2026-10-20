(in-package #:cl-yaml-kit/test)

(describe "format-preserving edit semantic and continuation behavior"
  (it "copies semantic YAML mappings recursively"
    (let* ((mapping (yaml-kit:make-yaml-mapping
                     (list (cons "a" 1) (cons "items" (list 10 20)))))
           (copy (yaml-kit::%format-edit-semantic-copy mapping)))
      (expect (yaml-kit:yaml-mapping-entries copy)
              :to-equal (yaml-kit:yaml-mapping-entries mapping))
      (expect (eq copy mapping) :to-be-falsy)
      (expect (eq (cdr (assoc "items" (yaml-kit:yaml-mapping-entries copy)
                              :test #'equal))
                  (cdr (assoc "items" (yaml-kit:yaml-mapping-entries mapping)
                              :test #'equal)))
              :to-be-falsy)))
  (it "replaces a semantic mapping entry without mutating the source"
    (let* ((mapping (yaml-kit:make-yaml-mapping
                     (list (cons "a" 1) (cons "items" (list 10 20)))))
           (original (yaml-kit:yaml-mapping-entries mapping))
           (replaced (yaml-kit::%format-edit-semantic-replace-child
                      mapping "a" 9 nil)))
      (expect (yaml-kit:yaml-mapping-entries replaced)
              :to-equal (list (cons "a" 9) (cons "items" (list 10 20))))
      (expect (yaml-kit:yaml-mapping-entries mapping) :to-equal original)))
  (it "adds a semantic mapping entry to the expected root"
    (let* ((mapping (yaml-kit:make-yaml-mapping
                     (list (cons "a" 1) (cons "items" (list 10 20)))))
           (expected-entries
             (append (yaml-kit:yaml-mapping-entries mapping)
                     (list (cons "c" 3))))
           (added (yaml-kit::%format-edit-semantic-add-child
                   mapping "c" 3 nil))
           (expected (yaml-kit::%format-edit-semantic-expected
                      (list mapping) '("c") 3 :add 0)))
      (expect (yaml-kit:yaml-mapping-entries added) :to-equal expected-entries)
      (expect (yaml-kit:yaml-mapping-entries (first expected))
              :to-equal expected-entries)))
  (it "deletes a semantic mapping entry without mutating the source"
    (let* ((mapping (yaml-kit:make-yaml-mapping
                     (list (cons "a" 1) (cons "items" (list 10 20)))))
           (original (yaml-kit:yaml-mapping-entries mapping))
           (deleted (yaml-kit::%format-edit-semantic-delete-child
                     mapping "a" nil)))
      (expect (yaml-kit:yaml-mapping-entries deleted)
              :to-equal (list (cons "items" (list 10 20))))
      (expect (yaml-kit:yaml-mapping-entries mapping) :to-equal original)))
  (it "replaces a semantic list item without mutating the source"
    (let* ((sequence '(10 20))
           (replaced (yaml-kit::%format-edit-semantic-replace-child
                      sequence 1 99 nil)))
      (expect replaced :to-equal '(10 99))
      (expect sequence :to-equal '(10 20))))
  (it "appends a semantic list item without mutating the source"
    (let* ((sequence '(10 20))
           (added (yaml-kit::%format-edit-semantic-add-child sequence 2 30 nil)))
      (expect added :to-equal '(10 20 30))
      (expect sequence :to-equal '(10 20))))
  (it "deletes a semantic list item without mutating the source"
    (let* ((sequence '(10 20))
           (deleted (yaml-kit::%format-edit-semantic-delete-child
                     sequence 0 nil)))
      (expect deleted :to-equal '(20))
      (expect sequence :to-equal '(10 20))))
  (it "reports a semantic mapping child and its presence"
    (let ((mapping (yaml-kit:make-yaml-mapping (list (cons "a" 1)))))
      (expect (multiple-value-list
               (yaml-kit::%format-edit-semantic-child mapping "a"))
              :to-equal '(1 t))))
  (it "reports a semantic sequence child and its presence"
    (let ((sequence '(10 20)))
      (expect (multiple-value-list
               (yaml-kit::%format-edit-semantic-child sequence 1))
              :to-equal '(20 t))))
  (it "reports a missing child for scalar values"
    (expect (multiple-value-list
             (yaml-kit::%format-edit-semantic-child 10 "a"))
            :to-equal '(nil nil)))
  (it "compares semantic YAML mappings by entries"
    (let* ((mapping (yaml-kit:make-yaml-mapping
                     (list (cons "a" 1) (cons "b" 2))))
           (copy (yaml-kit::%format-edit-semantic-copy mapping)))
      (expect (yaml-kit::%format-edit-semantic-equal-p mapping copy)
              :to-be-truthy)))
  (it "detects changed semantic YAML mapping entries"
    (let* ((mapping (yaml-kit:make-yaml-mapping
                     (list (cons "a" 1) (cons "b" 2))))
           (changed (yaml-kit::%format-edit-semantic-add-child
                     mapping "c" 3 nil)))
      (expect (yaml-kit::%format-edit-semantic-equal-p mapping changed)
              :to-be-falsy)))
  (it "compares semantic lists by elements"
    (let ((sequence '(10 20)))
      (expect (yaml-kit::%format-edit-semantic-equal-p sequence
                                                        (copy-list sequence))
              :to-be-truthy)))
  (it "does not equate semantic lists with vectors"
    (expect (yaml-kit::%format-edit-semantic-equal-p '(10 20) #(10 20))
            :to-be-falsy))
  (it "distinguishes numeric scalar types"
    (expect (yaml-kit::%format-edit-semantic-equal-p 3 3.0)
            :to-be-falsy))
  (it "detects unequal scalar leaves"
    (expect (yaml-kit::%format-edit-semantic-equal-p 3 4) :to-be-falsy))
  (it "detects numeric type changes in edited YAML"
    (expect (format-edit-condition-type
             (lambda ()
               (yaml-kit::%format-edit-validate-semantics
                '((1)) '((1.0)) '(0) 1 :set 0)))
            :to-equal 'yaml-kit:yaml-format-edit-structure-error))
  (it "calls edit-source/k once with the edited source"
    (let ((callback-count 0)
          (callback-source nil)
          (return-value nil))
      (setf return-value
            (yaml-kit:edit-source/k
             (format-edit-lf "a: old") '("a") "new"
             :k (lambda (edited)
                  (incf callback-count)
                  (setf callback-source edited)
                  :callback-result)))
      (expect callback-count :to-equal 1)
      (expect callback-source :to-equal (format-edit-lf "a: new"))
      (expect return-value :to-equal :callback-result)))
  (it "returns only the primary value from the edit-source/k callback"
    (expect
     (multiple-value-list
      (yaml-kit:edit-source/k
       (format-edit-lf "a: old") '("a") "new"
       :k (lambda (edited)
            (declare (ignore edited))
            (values :primary :secondary))))
     :to-equal '(:primary))))

(describe "format-preserving edit string equality"
  (it "compares semantic strings case-sensitively"
    (expect (yaml-kit::%format-edit-semantic-equal-p "lower" "UPPER")
            :to-be-falsy)))
