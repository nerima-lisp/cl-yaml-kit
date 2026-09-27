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

(defun loader-document (&rest events)
  (append (list (loader-event :stream-start)
                (loader-event :document-start))
          events
          (list (loader-event :document-end)
                (loader-event :stream-end))))

(defmacro loader-scalar-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (expect (yaml-kit::resolve-plain-scalar-tag
                             ,(third case) ,(second case))
                            :to-equal ,(fourth case))))
               cases)))

(defmacro loader-parse-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (expect (yaml-kit:parse
                             (loader-document
                              (loader-event :scalar :value ,(second case)))
                             ,@(third case))
                            :to-equal ,(fourth case))))
               cases)))

(describe "loader"
  (loader-scalar-cases
    ("core null" :core "" "tag:yaml.org,2002:null")
    ("core octal" :core "0o17" "tag:yaml.org,2002:int")
    ("core float" :core "1.25" "tag:yaml.org,2002:float")
    ("core bool" :core "true" "tag:yaml.org,2002:bool")
    ("json float" :json "1.25" "tag:yaml.org,2002:float")
    ("json trailing dot" :json "1." "tag:yaml.org,2002:str")
    ("json exponent trailing dot" :json "1.e2" "tag:yaml.org,2002:str")
    ("json leading zero" :json "01" "tag:yaml.org,2002:str")
    ("failsafe scalar" :failsafe "true" "tag:yaml.org,2002:str")
    ("core underscore scalar" :core "1_000" "tag:yaml.org,2002:str")
    ("core hexadecimal" :core "0x10" "tag:yaml.org,2002:int")
    ("core nan" :core ".NaN" "tag:yaml.org,2002:float"))

  (loader-parse-cases
    ("signed octal" "-0o17" () -15)
    ("signed hexadecimal" "+0x10" () 16)
    ("decimal float" "0.278" () 0.278d0)
    ("integral decimal float" "450.00" () 450))

  (it "parses all documents from an event list"
    (let ((events (append (loader-document
                           (loader-event :scalar :value "one"))
                          (list (loader-event :document-start)
                                (loader-event :scalar :value "two")
                                (loader-event :document-end)
                                (loader-event :stream-end)))))
      (expect (yaml-kit:parse-all events :schema :failsafe)
              :to-equal '("one" "two"))))

  (it "exposes compose-all and reports resource limits"
    (let ((events (loader-document
                   (loader-event :scalar :value "long"))))
      (expect (length (yaml-kit:compose-all events)) :to-equal 1)
      (expect (handler-case
                  (progn (yaml-kit:compose-all events :max-nodes 0) nil)
                (yaml-kit:yaml-resource-limit-error (condition)
                  (and (equal (yaml-kit::yaml-resource-limit-error-limit condition)
                              0)
                       (equal (yaml-kit::yaml-resource-limit-error-actual condition)
                              1))))
              :to-be-truthy)))

  (it "uses list and alist construction options"
    (let ((events (loader-document
                   (loader-event :sequence-start)
                   (loader-event :scalar :value "a")
                   (loader-event :scalar :value "b")
                   (loader-event :sequence-end))))
      (expect (yaml-kit:parse events :schema :failsafe :sequence-type :list)
              :to-equal '("a" "b")))
    (let ((events (loader-document
                   (loader-event :mapping-start)
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "value")
                   (loader-event :mapping-end))))
      (expect (yaml-kit:parse events :schema :failsafe :mapping-type :alist)
              :to-equal '(("key" . "value")))))

  (it "reads non-list input through the reader path"
    (with-input-from-string (stream "answer: 42")
      (let ((value (yaml-kit:read-yaml stream)))
        (expect (gethash "answer" value) :to-equal 42))))

  (it "preserves aliases as shared nodes during composition"
    (let* ((events (loader-document
                    (loader-event :mapping-start)
                    (loader-event :scalar :value "left")
                    (loader-event :sequence-start :anchor "a")
                    (loader-event :scalar :value "x")
                    (loader-event :sequence-end)
                    (loader-event :scalar :value "right")
                    (loader-event :alias :anchor "a")
                    (loader-event :mapping-end)))
           (node (yaml-kit:compose events)))
      (expect (eq (yaml-kit:sequence-node-items
                   (cdr (first (yaml-kit:mapping-node-pairs node))))
                  (yaml-kit:sequence-node-items
                   (cdr (second (yaml-kit:mapping-node-pairs node)))))
              :to-be-truthy)))

  (it "applies duplicate-key policies"
    (let ((events (loader-document
                   (loader-event :mapping-start)
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "first")
                   (loader-event :scalar :value "key")
                   (loader-event :scalar :value "last")
                   (loader-event :mapping-end))))
      (expect (gethash "key" (yaml-kit:parse events :duplicate-key-policy :first))
              :to-equal "first")
      (expect (gethash "key" (yaml-kit:parse events :duplicate-key-policy :last))
              :to-equal "last")
      (expect (handler-case (progn (yaml-kit:parse events) nil)
                (yaml-kit:yaml-compose-error () t))
              :to-be-truthy)))

  (it "rejects incompatible collection tags and enforces limits"
    (let ((events (loader-document
                   (loader-event :sequence-start :tag "!!str")
                   (loader-event :sequence-end))))
      (expect (handler-case (progn (yaml-kit:parse events) nil)
                (yaml-kit:yaml-compose-error () t))
              :to-be-truthy))
    (let ((events (loader-document (loader-event :scalar :value "long"))))
      (expect (handler-case
                  (progn (yaml-kit:parse events :max-scalar-length 3) nil)
                (yaml-kit:yaml-resource-limit-error () t))
              :to-be-truthy))))
