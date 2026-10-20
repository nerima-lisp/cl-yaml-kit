(in-package #:cl-yaml-kit/test)

(defun format-edit-invalid-container-generator ()
  "Generate YAML mappings and sequences that cannot be scalar replacements."
  (cl-weave:gen-one-of
   (cl-weave:gen-list (cl-weave:gen-integer :min -3 :max 3)
                      :min-length 1 :max-length 4)
   (cl-weave:gen-map
    (lambda (pairs)
      (yaml-kit:make-yaml-mapping
       (remove-duplicates pairs :test #'string= :key #'car)))
    (cl-weave:gen-list
     (cl-weave:gen-tuple
      (cl-weave:gen-string :min-length 1 :max-length 5 :alphabet "abcXYZ")
      (cl-weave:gen-integer :min -3 :max 3))
     :min-length 1 :max-length 3))))

(describe "format-preserving edit errors"
  (cl-weave:it-each
      ((collection ("a:" "  x: 1" "b: 2") ("a") "new")
       (block-scalar ("a: |" "  line1" "b: 2") ("a") "new")
       (multiline-scalar ("a: one" "  two" "b: 2") ("a") "new")
       (empty-scalar ("a:" "b: 2") ("a") "new")
       (tagged-scalar ("a: !!str 5" "b: 2") ("a") "new"))
    "rejects ~S scalar edit target"
    (name source-lines path value)
    (declare (ignore name))
    (expect (format-edit-condition-type
             (lambda ()
               (yaml-kit:edit-source
                (apply (function format-edit-lf) source-lines) path value)))
            :to-equal (quote yaml-kit:yaml-format-edit-structure-error)))
  (cl-weave:it-each
      ((mapping :mapping)
       (sequence :sequence))
    "rejects ~S replacement values"
    (name value-kind)
    (declare (ignore name))
    (let ((value (ecase value-kind
                   (:mapping (yaml-kit:make-yaml-mapping (list (cons "x" 1))))
                   (:sequence '(1 2)))))
      (expect (format-edit-condition-type
               (lambda ()
                 (yaml-kit:edit-source (format-edit-lf "a: old") '("a") value)))
              :to-equal 'yaml-kit:yaml-format-edit-structure-error)))
  (cl-weave:it-fuzz
    "rejects generated collection replacement values"
    ((value (format-edit-invalid-container-generator)))
    (:trials 100 :timeout-per-trial 5)
    (expect (format-edit-condition-type
             (lambda ()
               (yaml-kit:edit-source (format-edit-lf "a: old") '("a") value)))
            :to-equal 'yaml-kit:yaml-format-edit-structure-error))
  (cl-weave:it-each
      ((mapping-add ("- a: 1" "  b: 2" "- c: 3") (0 "z") 9 :set)
       (mapping-delete ("- a: 1" "  b: 2" "- c: 3") (0 "a") nil :delete)
       (sequence-add ("- - a" "  - b" "- c") (0 2) "z" :set)
       (sequence-delete ("- - a" "  - b" "- c") (0 0) nil :delete))
    "rejects structural edits on a shared sequence-marker line: ~S"
    (name source-lines path value operation)
    (declare (ignore name))
    (expect (format-edit-condition-type
             (lambda ()
               (yaml-kit:edit-source
                (apply (function format-edit-lf) source-lines)
                path value :operation operation)))
            :to-equal (quote yaml-kit:yaml-format-edit-structure-error)))
  (cl-weave:it-each
      ((tagged-scalar ("a: !!str 5") ("a")
                      yaml-kit:yaml-format-edit-structure-error)
       (duplicate-key ("a: 1" "a: 2") ("a")
                      yaml-kit:yaml-format-edit-structure-error)
       (merge-alias ("base: &b {x: 1}" "d:" "  <<: *b" "  y: 2")
                    ("d" "y") yaml-kit:yaml-format-edit-structure-error)
       (document-index ("a: 1") (0) yaml-kit:yaml-format-edit-path-error)
       (tagged-map ("root: !!map {a: 1}") ("root" "a")
                   yaml-kit:yaml-format-edit-structure-error)
       (anchored-node ("root: &x {a: 1}") ("root" "a")
                      yaml-kit:yaml-format-edit-anchor-error)
       (anchored-key ("&key a: 1") ("a")
                     yaml-kit:yaml-format-edit-anchor-error))
    "rejects an unsafe ~S edit path"
    (name source-lines path expected-type)
    (declare (ignore name))
    (expect (format-edit-condition-type
             (lambda ()
               (yaml-kit:edit-source
                (apply (function format-edit-lf) source-lines)
                path "new")))
            :to-equal expected-type))
  (cl-weave:it-each
      ((only-child ("a: 1") ("a") nil :delete)
       (flow-delete ("root: [a, b]") ("root" 0) nil :delete)
       (non-tail-add ("items:" "  - a" "  - b") ("items" 3) "x" :set))
    "rejects unsupported edit operation ~S"
    (name source-lines path value operation)
    (declare (ignore name))
    (expect (format-edit-condition-type
             (lambda ()
               (yaml-kit:edit-source
                (apply (function format-edit-lf) source-lines)
                path value :operation operation)))
            :to-equal (quote yaml-kit:yaml-format-edit-structure-error)))
  (it "rejects editing an anchored value"
    (expect (format-edit-condition-type
             (lambda ()
               (yaml-kit:edit-source
                (format-edit-lf "base: &base 1" "other: 2") '("base") 3)))
            :to-equal 'yaml-kit:yaml-format-edit-anchor-error))
  (it "allows editing a sibling outside an anchor"
    (format-edit-expect (format-edit-lf "base: &base 1" "other: 2")
                        '("other") 3
                        (format-edit-lf "base: &base 1" "other: 3")))
  (cl-weave:it-each
      ((replacement :set)
       (deletion :delete))
    "rejects alias ~S including merge aliases"
    (name operation)
    (declare (ignore name))
    (expect (format-edit-condition-type
             (lambda ()
               (yaml-kit:edit-source (format-edit-lf "a: &x 1" "b: *x")
                                     '("b") 2 :operation operation)))
            :to-equal 'yaml-kit:yaml-format-edit-anchor-error))
  (it "rejects a deletion that changes an unrelated alias value"
    (let ((source (format-edit-lf "a: &x 1" "b:" "  c: &x 2" "d: *x")))
      (expect (format-edit-condition-type
               (lambda ()
                 (yaml-kit:edit-source source '("b") nil :operation :delete)))
              :to-equal 'yaml-kit:yaml-format-edit-structure-error)))
  (cl-weave:it-each
      ((fffe #xfffe)
       (ffff #xffff)
       (high-surrogate #xd800)
       (low-surrogate #xdfff))
    "rejects YAML non-scalar U+~4,'0X in replacement strings"
    (name code)
    (declare (ignore name))
    (let* ((path '("value"))
           (condition
             (format-edit-condition
              (lambda ()
                (yaml-kit:edit-source
                 (format-edit-lf "value: old")
                 path
                 (string (code-char code)))))))
      (expect (type-of condition)
              :to-equal 'yaml-kit:yaml-format-edit-structure-error)
      (expect (yaml-kit:yaml-format-edit-structure-error-path condition)
              :to-equal path)))
  (it "converts invalid octets to a YAML parse condition"
    (let ((octets (coerce #(255 0 1) '(vector (unsigned-byte 8)))))
      (expect (format-edit-condition-type
               (lambda ()
                 (yaml-kit:edit-source octets '("a") 1
                                       :document 4 :operation :delete)))
              :to-equal 'yaml-kit:yaml-parse-error)))
  (it "rejects unsupported input types"
    (expect (format-edit-condition-type
             (lambda ()
               (yaml-kit:edit-source 42 '("a") 1
                                     :document 4 :operation :delete)))
            :to-equal 'yaml-kit:yaml-format-edit-path-error))
  (it "reports the input-type error document and operation"
    (let ((condition (format-edit-condition
                     (lambda ()
                       (yaml-kit:edit-source 42 '("a") 1
                                             :document 4 :operation :delete)))))
      (expect (type-of condition) :to-equal 'yaml-kit:yaml-format-edit-path-error)
      (expect (yaml-kit:yaml-format-edit-path-error-document condition) :to-equal 4)
      (expect (yaml-kit:yaml-format-edit-path-error-operation condition)
              :to-equal :delete)))
  (it "accepts simple base-string input"
    (format-edit-expect (format nil "a: old~%b: 2~%") '("a") "new"
                        (format nil "a: new~%b: 2~%")
                        :unchanged (list '("b") 2)))
  (it "does not call the edit-source/k callback when editing fails"
    (let ((callback-count 0))
      (expect (format-edit-condition-type
               (lambda ()
                 (yaml-kit:edit-source/k
                  (format-edit-lf "a: |" "  line1") '("a") "new"
                  :k (lambda (edited)
                       (declare (ignore edited))
                       (incf callback-count)))))
              :to-equal 'yaml-kit:yaml-format-edit-structure-error)
      (expect callback-count :to-equal 0))))
