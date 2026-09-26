;;;; t/loader-test.lisp
(in-package #:cl-yaml-kit/test)

(defun loader-event (type &rest options)
  (apply (ecase type
           (:stream-start #'yaml-kit:make-stream-start-event)
           (:stream-end #'yaml-kit:make-stream-end-event)
           (:document-start #'yaml-kit:make-document-start-event)
           (:document-end #'yaml-kit:make-document-end-event)
           (:scalar #'yaml-kit:make-scalar-event)
           (:sequence-start #'yaml-kit:make-sequence-start-event)
           (:sequence-end #'yaml-kit:make-sequence-end-event)
           (:mapping-start #'yaml-kit:make-mapping-start-event)
           (:mapping-end #'yaml-kit:make-mapping-end-event)
           (:alias #'yaml-kit:make-alias-event))
         options))

(describe "loader"
  (it "resolves the YAML 1.2.2 scalar tables"
    (dolist (case '((:core "" "tag:yaml.org,2002:null")
                    (:core "0o17" "tag:yaml.org,2002:int")
                    (:core "1.25" "tag:yaml.org,2002:float")
                    (:core "true" "tag:yaml.org,2002:bool")
                    (:json "1.25" "tag:yaml.org,2002:float")
                    (:json "01" "tag:yaml.org,2002:str")
                    (:failsafe "true" "tag:yaml.org,2002:str")))
      (destructuring-bind (schema text expected) case
      (expect (yaml-kit::resolve-plain-scalar-tag text schema) :to-equal expected))))

  (it "keeps the schema resolution table data-driven"
    (dolist (case '((:failsafe "true" "tag:yaml.org,2002:str")
                    (:json "null" "tag:yaml.org,2002:null")
                    (:json "01" "tag:yaml.org,2002:str")
                    (:core "1_000" "tag:yaml.org,2002:str")
                    (:core "0x10" "tag:yaml.org,2002:int")
                    (:core ".NaN" "tag:yaml.org,2002:float")))
      (destructuring-bind (schema text expected) case
        (expect (yaml-kit::resolve-plain-scalar-tag text schema)
                :to-equal expected))))

  (it "constructs signed base-prefixed integers"
    (dolist (case '(("-0o17" -15) ("+0o17" 15)
                    ("-0x10" -16) ("+0x10" 16)))
      (destructuring-bind (text expected) case
        (expect (yaml-kit:parse
                 (list (loader-event :stream-start)
                       (loader-event :document-start)
                       (loader-event :scalar :value text)
                       (loader-event :document-end)
                       (loader-event :stream-end)))
                :to-equal expected))))

  (it "reads decimal floats at double-float precision"
    (let ((value (yaml-kit:parse
                  (list (loader-event :stream-start)
                        (loader-event :document-start)
                        (loader-event :scalar :value "0.278")
                        (loader-event :document-end)
                        (loader-event :stream-end)))))
      (expect value :to-equal 0.278d0)))

  (it "normalizes implicit integral decimal floats"
    (dolist (case '(("450.00" 450) ("2392.00" 2392)
                    ("450.25" 450.25d0)))
      (destructuring-bind (text expected) case
        (expect (yaml-kit:parse
                 (list (loader-event :stream-start)
                       (loader-event :document-start)
                       (loader-event :scalar :value text)
                       (loader-event :document-end)
                       (loader-event :stream-end)))
                :to-equal expected))))

  (it "rejects incompatible explicit collection tags"
    (dolist (case '((:sequence "!!str") (:mapping "!!seq")))
      (destructuring-bind (kind tag) case
        (let ((events (list (loader-event :stream-start)
                            (loader-event :document-start)
                            (if (eq kind :sequence)
                                (loader-event :sequence-start :tag tag)
                                (loader-event :mapping-start :tag tag))
                            (if (eq kind :sequence)
                                (loader-event :sequence-end)
                                (loader-event :mapping-end))
                            (loader-event :document-end)
                            (loader-event :stream-end))))
          (expect (handler-case (progn (yaml-kit:parse events) nil)
                    (yaml-kit:yaml-compose-error () t))
                  :to-be-truthy)))))

  (it "constructs a scalar from events"
    (let ((value (yaml-kit:parse
                  (list (loader-event :stream-start)
                        (loader-event :document-start)
                        (loader-event :scalar :value "42")
                        (loader-event :document-end)
                        (loader-event :stream-end)))))
      (expect value :to-equal 42)))
  (it "preserves aliases as shared nodes during composition"
    (let* ((events (list (loader-event :stream-start)
                         (loader-event :document-start)
                         (loader-event :mapping-start)
                         (loader-event :scalar :value "left")
                         (loader-event :sequence-start :anchor "a")
                         (loader-event :scalar :value "x")
                         (loader-event :sequence-end)
                         (loader-event :scalar :value "right")
                         (loader-event :alias :anchor "a")
                         (loader-event :mapping-end)
                         (loader-event :document-end)
                         (loader-event :stream-end)))
           (node (yaml-kit:compose events)))
      (expect (yaml-kit:mapping-node-p node) :to-be-truthy)
      (expect (eq (yaml-kit:sequence-node-items
                   (cdr (first (yaml-kit:mapping-node-pairs node))))
                  (yaml-kit:sequence-node-items
                   (cdr (second (yaml-kit:mapping-node-pairs node)))))
              :to-be-truthy))))

  (it "applies duplicate-key policies"
    (let ((events (list (loader-event :stream-start)
                        (loader-event :document-start)
                        (loader-event :mapping-start)
                        (loader-event :scalar :value "key")
                        (loader-event :scalar :value "first")
                        (loader-event :scalar :value "key")
                        (loader-event :scalar :value "last")
                        (loader-event :mapping-end)
                        (loader-event :document-end)
                        (loader-event :stream-end))))
      (expect (gethash "key" (yaml-kit:parse events :duplicate-key-policy :first))
              :to-equal "first")
      (expect (gethash "key" (yaml-kit:parse events :duplicate-key-policy :last))
              :to-equal "last")
      (expect (handler-case
                  (progn (yaml-kit:parse events) nil)
                (yaml-kit:yaml-compose-error (condition)
                  (declare (ignore condition))
                  t))
              :to-be-truthy)))

  (it "enforces loader limits for event lists"
    (let ((events (list (loader-event :stream-start)
                        (loader-event :document-start)
                        (loader-event :scalar :value "long")
                        (loader-event :document-end)
                        (loader-event :stream-end))))
      (expect (handler-case
                  (progn (yaml-kit:parse events :max-scalar-length 3) nil)
                (yaml-kit:yaml-resource-limit-error (condition)
                  (declare (ignore condition))
                  t))
              :to-be-truthy)))
