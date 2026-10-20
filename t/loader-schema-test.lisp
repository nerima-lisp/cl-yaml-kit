;;;; t/loader-schema-test.lisp
(in-package #:cl-yaml-kit/test)

(defvar yaml-kit::*probe-tag-table*)

(describe "loader schema"
  (it "evaluates the tag-table definition macro"
    (eval '(yaml-kit::define-tag-table yaml-kit::*probe-tag-table*
             ("!probe" "tag:yaml.org,2002:probe" :probe :scalar)))
    (expect (second (first yaml-kit::*probe-tag-table*))
            :to-equal "tag:yaml.org,2002:probe")
    (expect (lambda ()
              (macroexpand
               '(yaml-kit::define-tag-table yaml-kit::*probe-tag-table*
                  ("!bad" "tag:bad" :bad :invalid))))
            :to-signal-loader-error
            'simple-error))
  (it "compiles initialized schema tables and resolves matching and non-matching scalars"
    (let* ((tables
             (mapcar (lambda (schema)
                       (cons (first schema)
                             (yaml-kit::%compile-schema-table (rest schema))))
                     yaml-kit::*schema-spec-tables*))
           (core-table (cdr (assoc :core tables)))
           (json-table (cdr (assoc :json tables)))
           (failsafe-table (cdr (assoc :failsafe tables))))
      (expect (mapcar (function car)
                      yaml-kit::*compiled-schema-tables*)
              :to-equal (list :failsafe :json :core))
      (expect (list (yaml-kit::%schema-resolve "true" core-table)
                    (yaml-kit::%schema-resolve "trueish" core-table))
              :to-equal '("tag:yaml.org,2002:bool" "tag:yaml.org,2002:str"))
      (expect (list (yaml-kit::%schema-resolve "null" json-table)
                    (yaml-kit::%schema-resolve "true" failsafe-table))
              :to-equal '("tag:yaml.org,2002:null" "tag:yaml.org,2002:str"))
      (let ((regexp
              (cl-regex-kit:compile-regex
               (yaml-kit::%schema-spellings-regex '("a.b" "x+y")))))
        (expect (and (cl-regex-kit:full-match-p regexp "a.b")
                     (cl-regex-kit:full-match-p regexp "x+y")
                     (not (cl-regex-kit:full-match-p regexp "axb"))
                     (not (cl-regex-kit:full-match-p regexp "xy")))
                :to-be-truthy))))

  (cl-weave:it-each
      ((core-null :core "" "tag:yaml.org,2002:null")
       (core-octal :core "0o17" "tag:yaml.org,2002:int")
       (core-float :core "1.25" "tag:yaml.org,2002:float")
       (core-bool :core "true" "tag:yaml.org,2002:bool")
       (json-float :json "1.25" "tag:yaml.org,2002:float")
       (json-trailing-dot :json "1." "tag:yaml.org,2002:str")
       (json-exponent-trailing-dot :json "1.e2" "tag:yaml.org,2002:str")
       (json-leading-zero :json "01" "tag:yaml.org,2002:str")
       (failsafe-scalar :failsafe "true" "tag:yaml.org,2002:str")
       (core-underscore :core "1_000" "tag:yaml.org,2002:str")
       (core-hexadecimal :core "0x10" "tag:yaml.org,2002:int")
       (core-nan :core ".NaN" "tag:yaml.org,2002:float")
       (core-positive-infinity :core ".inf" "tag:yaml.org,2002:float")
       (core-negative-infinity :core "-.Inf" "tag:yaml.org,2002:float")
       (json-null :json "null" "tag:yaml.org,2002:null")
       (json-boolean :json "false" "tag:yaml.org,2002:bool")
       (schema-fast-path :core "#" "tag:yaml.org,2002:str")
       (schema-unicode-fast-path :core "あ" "tag:yaml.org,2002:str"))
    "resolves scalar tag for ~S"
    (name schema text expected)
    (declare (ignore name))
    (expect (yaml-kit::resolve-plain-scalar-tag text schema)
            :to-equal
            expected))

  (cl-weave:it-each
      ((signed-octal "-0o17" -15)
       (signed-hexadecimal "+0x10" 16)
       (decimal-float "0.278" 0.278d0)
       (integral-decimal-float "450.00" 450)
       (leading-dot-float ".5" 0.5d0)
       (trailing-dot-float "1." 1)
       (exponent-float "1e3" 1000)
       (underflowing-float "1e-400" 0))
    "parses scalar as ~S"
    (name input expected)
    (declare (ignore name))
    (expect input :to-parse-as expected))

  (cl-weave:it-each
      ((unknown-schema :parse-document
         ((:scalar :value "true"))
         (:schema :unknown)
         yaml-kit:yaml-compose-error))
    "rejects schema case ~S"
    (name operation input options expected)
    (declare (ignore name))
    (expect (lambda () (loader-case-call operation input options))
            :to-signal-loader-error
            expected))

  (cl-weave:it-each
      ((positive-infinity ".INF" :positive)
       (negative-infinity "-.INF" :negative))
    "constructs ~S as an infinity"
    (name input expected-sign)
    (declare (ignore name))
    (let ((value (yaml-kit:parse input)))
      (expect (and (floatp value)
                   (yaml-kit::%float-infinity-p value)
                   (case expected-sign
                     (:positive (not (minusp value)))
                     (:negative (minusp value))))
              :to-be-truthy)))

  (cl-weave:it-each
      ((empty-input :parse-all "" () nil)
       (empty-sequence-list :parse-document
         ((:sequence-start) (:sequence-end))
         (:sequence-type :list)
         nil))
    "returns ~S for empty value case ~S"
    (name operation input options expected)
    (declare (ignore name))
    (expect (loader-case-call operation input options)
            :to-equal
            expected))

  (cl-weave:it-each
      ((explicit-string :scalar "true" "!!str" :plain :core
        "tag:yaml.org,2002:str")
       (explicit-integer :scalar "1" "!!int" :plain :core
        "tag:yaml.org,2002:int")
       (explicit-float :scalar "1.0" "!!float" :plain :core
        "tag:yaml.org,2002:float")
       (explicit-boolean :scalar "true" "!!bool" :plain :core
        "tag:yaml.org,2002:bool")
       (explicit-null :scalar "null" "!!null" :plain :core
        "tag:yaml.org,2002:null")
       (explicit-sequence :sequence nil "!!seq" nil :core
        "tag:yaml.org,2002:seq")
       (explicit-mapping :mapping nil "!!map" nil :core
        "tag:yaml.org,2002:map")
       (non-specific-string :scalar "true" "!" :plain :core
        "tag:yaml.org,2002:str")
       (non-specific-core-boolean :scalar "true" "?" :plain :core
        "tag:yaml.org,2002:bool")
       (implicit-sequence :sequence nil nil nil :core
        "tag:yaml.org,2002:seq")
       (implicit-mapping :mapping nil nil nil :core
        "tag:yaml.org,2002:map"))
    "resolves tag for ~S"
    (name kind value tag style schema expected)
    (declare (ignore name))
    (let ((node
            (ecase kind
              (:scalar
               (yaml-kit:make-scalar-node :value value :tag tag :style style))
              (:sequence (yaml-kit:make-sequence-node :tag tag))
              (:mapping (yaml-kit:make-mapping-node :tag tag)))))
      (expect (yaml-kit::resolve-tag node :schema schema)
              :to-equal
              expected)))
  (it "preserves schema-data constant identities on reload"
    (let* ((symbols
             (quote (yaml-kit::+yaml-schema-names+
                     yaml-kit::+yaml-json-null-spellings+
                     yaml-kit::+yaml-json-true-spellings+
                     yaml-kit::+yaml-json-false-spellings+
                     yaml-kit::+yaml-core-null-spellings+
                     yaml-kit::+yaml-core-true-spellings+
                     yaml-kit::+yaml-core-false-spellings+
                     yaml-kit::+yaml-schema-string-tag+
                     yaml-kit::+yaml-schema-sequence-tag+
                     yaml-kit::+yaml-schema-mapping-tag+)))
           (before (mapcar (function symbol-value) symbols)))
      (let* ((probe (make-symbol "SCHEMA-DATA-PROBE"))
             (initial-value (list :initial))
             (existing-value (list :existing)))
        (expect (eq (yaml-kit::%reload-safe-schema-constant
                     probe
                     (lambda () initial-value))
                    initial-value)
                :to-be-truthy)
        (progv (list probe) (list existing-value)
          (expect (eq (yaml-kit::%reload-safe-schema-constant
                       probe
                       (lambda ()
                         (error "Initializer called for a bound symbol.")))
                      existing-value)
                  :to-be-truthy)))
      (load (asdf:system-relative-pathname :cl-yaml-kit "src/schema-data.lisp"))
      (expect (every (function eq) before (mapcar (function symbol-value) symbols))
              :to-be-truthy))))
