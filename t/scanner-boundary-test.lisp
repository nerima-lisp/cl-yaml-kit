(in-package #:cl-yaml-kit/test)
(defparameter *invalid-token-slot-value* -1)

(describe "scanner token errors"
  (cl-weave:it-each
      ((required-key-crosses-line #.(format nil "a~%b: c"))
       (flow-entry-before-comment "[item,#]"))
    "signals a YAML parse error for scanner input ~S"
    (name text)
    (declare (ignore name))
    (expect (nth-value 1
                   (ignore-errors (scanner-token-kinds text)))
        :to-be-type-of 'yaml-kit:yaml-parse-error))

  (it "rejects a flow entry when called outside flow context"
    (let ((scanner (yaml-kit::make-scanner (scanner-source ","))))
      (expect (nth-value 1 (ignore-errors (yaml-kit::fetch-flow-entry scanner)))
              :to-be-type-of 'yaml-kit:yaml-parse-error)))

  (it "rejects a required simple key when it is removed"
    (let* ((scanner (yaml-kit::make-scanner (scanner-source "key")))
           (key (first (yaml-kit::scanner-simple-keys scanner))))
      (setf (yaml-kit::simple-key-possible key) t
            (yaml-kit::simple-key-required key) t
            (yaml-kit::simple-key-position key) (yaml-kit::sc-position-at 0 0 0))
      (expect (nth-value 1 (ignore-errors (yaml-kit::remove-simple-key scanner)))
              :to-be-type-of 'yaml-kit:yaml-parse-error)))

  (it "reports a token queue resource limit"
    (let ((scanner (yaml-kit::make-scanner (scanner-source ""))))
      (dotimes (i 65) (vector-push-extend nil (yaml-kit::scanner-tokens scanner)))
      (setf (yaml-kit::scanner-tokens-head scanner) 65
            (yaml-kit::scanner-tokens-parsed scanner) 65)
      (expect (nth-value 1 (ignore-errors (yaml-kit::fetch-more-tokens scanner)))
              :to-be-type-of 'yaml-kit:yaml-resource-limit-error)))

  (it "skips a BOM before the first token"
    (expect (scanner-token-kinds (format nil "~Cvalue" #\UFEFF))
            :to-equal '(:stream-start :scalar :stream-end)))

  (it "does not fetch after stream end is produced"
    (let ((scanner (yaml-kit::make-scanner (scanner-source ""))))
      (setf (yaml-kit::scanner-stream-end-produced scanner) t)
      (expect (yaml-kit::fetch-more-tokens scanner) :to-equal nil)))

  (cl-weave:it-each
      ((stale-line 0 0 1 0 nil)
       (stale-offset 0 0 0 1025 nil)
       (current-position 0 0 0 0 t))
    "updates simple-key candidacy at ~S"
    (name position-line position-offset current-line current-position expected-possible-p)
    (declare (ignore name))
    (let* ((scanner (yaml-kit::make-scanner (scanner-source "x")))
           (key (first (yaml-kit::scanner-simple-keys scanner))))
      (setf (yaml-kit::simple-key-possible key) t
            (yaml-kit::simple-key-required key) nil
            (yaml-kit::simple-key-position key)
            (yaml-kit::sc-position-at position-offset position-line 0)
            (yaml-kit::scanner-line scanner) current-line
            (yaml-kit::scanner-pos scanner) current-position)
      (yaml-kit::stale-simple-keys scanner)
      (expect (yaml-kit::simple-key-possible key) :to-be expected-possible-p)))

  (it "removes an optional simple key"
    (let* ((scanner (yaml-kit::make-scanner (scanner-source "x")))
           (key (first (yaml-kit::scanner-simple-keys scanner))))
      (setf (yaml-kit::simple-key-possible key) t
            (yaml-kit::simple-key-required key) nil)
      (yaml-kit::remove-simple-key scanner)
      (expect (yaml-kit::simple-key-possible key) :to-be-falsy))))

(describe "token payload validation"
  (cl-weave:it-each
      ((single-quoted-scalar :scalar :single-quoted t)
       (double-quoted-scalar :scalar :double-quoted t)
       (flow-sequence-end :flow-sequence-end nil t)
       (flow-mapping-end :flow-mapping-end nil t)
       (plain-scalar :scalar :plain nil)
       (alias :alias nil nil))
    "classifies JSON-like node ending token ~S"
    (name kind style expected)
    (declare (ignore name))
    (let ((span (test-point-span 0 0 0)))
      (expect (yaml-kit::token-ends-json-like-node-p
               (yaml-kit:make-token kind span span :style style))
              :to-equal expected)))

  (cl-weave:it-each
      ((scalar-value :scalar-value)
       (tag-handle :tag-handle)
       (tag-suffix :tag-suffix)
       (scalar-style :scalar-style)
       (private-scalar-value :private-scalar-value)
       (private-tag-handle :private-tag-handle)
       (private-tag-suffix :private-tag-suffix)
       (private-scalar-style :private-scalar-style))
    "rejects invalid token payload ~S"
    (name operation)
    (declare (ignore name))
    (let* ((span (test-point-span 0 0 0))
           (condition
             (nth-value
              1
              (ignore-errors
                (ecase operation
                  (:scalar-value
                   (yaml-kit:make-token :scalar span span :value 1))
                  (:tag-handle
                   (yaml-kit:make-token :tag span span :handle 1))
                  (:tag-suffix
                   (yaml-kit:make-token :tag span span :suffix 1))
                  (:scalar-style
                   (yaml-kit:make-token :scalar span span :style :invalid))
                  (:private-scalar-value
                   (yaml-kit::%make-token :scalar span span
                                          :value *invalid-token-slot-value*))
                  (:private-tag-handle
                   (yaml-kit::%make-token :tag span span
                                          :handle *invalid-token-slot-value*))
                  (:private-tag-suffix
                   (yaml-kit::%make-token :tag span span
                                          :suffix *invalid-token-slot-value*))
                  (:private-scalar-style
                   (yaml-kit::%make-token :scalar span span
                                          :style *invalid-token-slot-value*)))))))
      (expect condition :to-be-type-of 'type-error)))

  (cl-weave:it-each
      ((invalid-kind)
       (invalid-start-span)
       (invalid-end-span)
       (invalid-major)
       (invalid-minor))
    "rejects invalid token metadata ~S"
    (operation)
    (let* ((span (test-point-span 0 0 0))
           (condition
             (nth-value
              1
              (ignore-errors
                (ecase operation
                  (invalid-kind (yaml-kit:make-token 1 span span))
                  (invalid-start-span (yaml-kit:make-token :scalar 1 span))
                  (invalid-end-span (yaml-kit:make-token :scalar span 1))
                  (invalid-major
                   (yaml-kit:make-token :version-directive span span :major t))
                  (invalid-minor
                   (yaml-kit:make-token :version-directive span span :minor t)))))))
      (expect condition :to-be-type-of 'type-error)))

  (it "constructs tokens with each valid payload variant"
    (let* ((start-span (test-point-span 1 2 3))
           (end-span (test-point-span 1 8 9))
           (scalar (yaml-kit:make-token :scalar start-span end-span
                                        :value "quoted" :style :double-quoted))
           (tag (yaml-kit:make-token :tag start-span end-span
                                     :handle "!e!" :suffix "item"))
           (version (yaml-kit:make-token :version-directive start-span end-span
                                         :major 1 :minor 2)))
      (expect (list (list (yaml-kit:token-kind scalar)
                          (yaml-kit:token-value scalar)
                          (yaml-kit:token-handle scalar)
                          (yaml-kit:token-suffix scalar)
                          (yaml-kit:token-style scalar)
                          (yaml-kit:token-major scalar)
                          (yaml-kit:token-minor scalar)
                          (eq (yaml-kit:token-start-span scalar) start-span)
                          (eq (yaml-kit:token-end-span scalar) end-span))
                    (list (yaml-kit:token-kind tag)
                          (yaml-kit:token-value tag)
                          (yaml-kit:token-handle tag)
                          (yaml-kit:token-suffix tag)
                          (yaml-kit:token-style tag)
                          (yaml-kit:token-major tag)
                          (yaml-kit:token-minor tag)
                          (eq (yaml-kit:token-start-span tag) start-span)
                          (eq (yaml-kit:token-end-span tag) end-span))
                    (list (yaml-kit:token-kind version)
                          (yaml-kit:token-value version)
                          (yaml-kit:token-handle version)
                          (yaml-kit:token-suffix version)
                          (yaml-kit:token-style version)
                          (yaml-kit:token-major version)
                          (yaml-kit:token-minor version)
                          (eq (yaml-kit:token-start-span version) start-span)
                          (eq (yaml-kit:token-end-span version) end-span)))
              :to-equal '((:scalar "quoted" nil nil :double-quoted nil nil t t)
                          (:tag nil "!e!" "item" nil nil nil t t)
                          (:version-directive nil nil nil nil 1 2 t t))))))
