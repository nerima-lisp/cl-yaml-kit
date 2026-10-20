;;;; t/conditions-test.lisp
(in-package #:cl-yaml-kit/test)

(progn
  (defun test-diagnostic (message &optional span)
    (cl-parser-kit:make-diagnostic
     :kind :error :message message :span span))
  (defun condition-report (condition)
    (with-output-to-string (stream)
      (princ condition stream))))

(defun condition-contract-example (name)
  (ecase name
    (:yaml-parse-error
     (values
      (make-condition 'yaml-kit:yaml-parse-error
                      :diagnostic (test-diagnostic
                                   "parse detail" (test-point-span 4 5 6))
                      :context "context")
      (list (cons #'yaml-kit:yaml-parse-error-context "context"))
      '("parse detail")))
    (:yaml-compose-error
     (values
      (make-condition 'yaml-kit:yaml-compose-error
                      :diagnostic (test-diagnostic
                                   "compose detail" (test-point-span 7 8 9))
                      :context "compose" :cause "cause")
      (list (cons #'yaml-kit:yaml-compose-error-context "compose")
            (cons #'yaml-kit:yaml-compose-error-cause "cause"))
      '("compose detail")))
    (:yaml-emit-error
     (values
      (make-condition 'yaml-kit:yaml-emit-error
                      :diagnostic (test-diagnostic
                                   "emit detail" (test-point-span 10 11 12))
                      :context "emit" :cause "cause")
      (list (cons #'yaml-kit:yaml-emit-error-context "emit")
            (cons #'yaml-kit:yaml-emit-error-cause "cause"))
      '("emit detail")))
    (:yaml-resource-limit-error
     (values
      (make-condition 'yaml-kit:yaml-resource-limit-error
                      :diagnostic (test-diagnostic
                                   "resource detail" (test-point-span 13 14 15))
                      :limit-name "depth" :limit 3 :actual 4 :context "resource")
      (list (cons #'yaml-kit::yaml-resource-limit-error-limit-name "depth")
            (cons #'yaml-kit::yaml-resource-limit-error-limit 3)
            (cons #'yaml-kit::yaml-resource-limit-error-actual 4)
            (cons #'yaml-kit::yaml-resource-limit-error-context "resource"))
      '("resource detail")))
    (:yaml-format-edit-path-error
     (values
      (make-condition 'yaml-kit:yaml-format-edit-path-error
                      :diagnostic (test-diagnostic "path detail")
                      :path '("a") :document 1 :operation :delete)
      (list (cons #'yaml-kit:yaml-format-edit-path-error-path '("a"))
            (cons #'yaml-kit:yaml-format-edit-path-error-document 1)
            (cons #'yaml-kit:yaml-format-edit-path-error-operation :delete))
      '("path detail")))
    (:yaml-format-edit-structure-error
     (values
      (make-condition 'yaml-kit:yaml-format-edit-structure-error
                      :diagnostic (test-diagnostic "structure detail")
                      :path '("a" 0))
      (list (cons #'yaml-kit:yaml-format-edit-structure-error-path '("a" 0)))
      '("structure detail")))
    (:yaml-format-edit-anchor-error
     (values
      (make-condition 'yaml-kit:yaml-format-edit-anchor-error
                      :diagnostic (test-diagnostic "anchor detail")
                      :path '("a") :anchor "base")
      (list (cons #'yaml-kit:yaml-format-edit-anchor-error-path '("a"))
            (cons #'yaml-kit:yaml-format-edit-anchor-error-anchor "base"))
      '("anchor detail")))))

(describe "condition contract"
  (it "retains the base error type and diagnostic"
    (let* ((diagnostic (test-diagnostic "base"))
           (condition
             (make-condition 'yaml-kit:yaml-kit-error
                             :diagnostic diagnostic)))
      (expect (typep condition 'error) :to-be-truthy)
      (expect (eq (yaml-kit:yaml-kit-error-diagnostic condition)
                  diagnostic)
              :to-be-truthy)))
  (it "reports the base diagnostic text"
    (let* ((diagnostic (test-diagnostic "base"))
           (condition
             (make-condition 'yaml-kit:yaml-kit-error
                             :diagnostic diagnostic))
           (report (condition-report condition)))
      (expect (search "base" report) :to-be-truthy)
      (expect report :to-equal
              (cl-parser-kit:diagnostic->string diagnostic))))
  (it "reports default diagnostics without spans"
    (dolist (condition
              (list (make-condition 'yaml-kit:yaml-kit-error)
                    (make-condition 'yaml-kit:yaml-compose-error)
                    (make-condition 'yaml-kit:yaml-emit-error)
                    (make-condition 'yaml-kit:yaml-resource-limit-error)))
      (let ((diagnostic (yaml-kit:yaml-kit-error-diagnostic condition)))
        (expect (cl-parser-kit:diagnostic-span diagnostic) :to-equal nil)
        (expect (search "YAML error" (condition-report condition))
                :to-be-truthy))))
  (it "keeps compose error context and cause readers"
    (let ((condition
            (make-condition 'yaml-kit:yaml-compose-error
                            :diagnostic (test-diagnostic "compose")
                            :context "compose context" :cause "compose cause")))
      (expect (yaml-kit:yaml-compose-error-context condition)
              :to-equal "compose context")
      (expect (yaml-kit:yaml-compose-error-cause condition)
              :to-equal "compose cause")))
  (it "keeps emit error context and cause readers"
    (let ((condition
            (make-condition 'yaml-kit:yaml-emit-error
                            :diagnostic (test-diagnostic "emit")
                            :context "emit context" :cause "emit cause")))
      (expect (yaml-kit:yaml-emit-error-context condition)
              :to-equal "emit context")
      (expect (yaml-kit:yaml-emit-error-cause condition)
              :to-equal "emit cause")))
  (it "formats compose error diagnostics"
    (let ((condition
            (make-condition 'yaml-kit:yaml-compose-error
                            :diagnostic (test-diagnostic "compose"))))
      (expect (condition-report condition) :to-equal
              (cl-parser-kit:diagnostic->string
               (yaml-kit:yaml-kit-error-diagnostic condition)))))
  (it "formats emit error diagnostics"
    (let ((condition
            (make-condition 'yaml-kit:yaml-emit-error
                            :diagnostic (test-diagnostic "emit"))))
      (expect (condition-report condition) :to-equal
              (cl-parser-kit:diagnostic->string
               (yaml-kit:yaml-kit-error-diagnostic condition)))))
  (it "stringifies non-string compose error messages"
    (let ((condition
            (handler-case
                (yaml-kit::signal-yaml-compose-error :message :unknown)
              (yaml-kit:yaml-compose-error (condition) condition))))
      (expect (cl-parser-kit:diagnostic-message
               (yaml-kit:yaml-kit-error-diagnostic condition))
              :to-equal "UNKNOWN")))
  (it "signals compose errors through the helper"
    (let ((condition
            (handler-case
                (yaml-kit::signal-yaml-compose-error
                 :message "compose helper" :cause "cause")
              (yaml-kit:yaml-compose-error (condition) condition))))
      (expect (cl-parser-kit:diagnostic-message
               (yaml-kit:yaml-kit-error-diagnostic condition))
              :to-equal "compose helper")
      (expect (yaml-kit:yaml-compose-error-cause condition)
              :to-equal "cause")))
  (it "signals emit errors through the helper"
    (let ((condition
            (handler-case
                (yaml-kit::signal-yaml-emit-error
                 :diagnostic (test-diagnostic "emit helper")
                 :context "emit context")
              (yaml-kit:yaml-emit-error (condition) condition))))
      (expect (cl-parser-kit:diagnostic-message
               (yaml-kit:yaml-kit-error-diagnostic condition))
              :to-equal "emit helper")
      (expect (yaml-kit:yaml-emit-error-context condition)
              :to-equal "emit context")))
  (it-each
      ((:yaml-parse-error)
       (:yaml-compose-error)
       (:yaml-emit-error)
       (:yaml-resource-limit-error)
       (:yaml-format-edit-path-error)
       (:yaml-format-edit-structure-error)
       (:yaml-format-edit-anchor-error))
    "reports and exposes condition ~S"
    (name)
    (multiple-value-bind (condition accessors expected)
        (condition-contract-example name)
      (let* ((diagnostic (yaml-kit:yaml-kit-error-diagnostic condition))
             (report (condition-report condition)))
        (expect (typep condition 'yaml-kit:yaml-kit-error)
                :to-be-truthy)
        (expect (and (typep diagnostic 'cl-parser-kit:diagnostic)
                     (stringp (cl-parser-kit:diagnostic-message diagnostic))
                     (string= report (cl-parser-kit:diagnostic->string diagnostic))
                     (every (lambda (value) (search value report)) expected))
                :to-be-truthy)
        (expect (every (lambda (accessor-expectation)
                         (equal (funcall (car accessor-expectation) condition)
                                (cdr accessor-expectation)))
                       accessors)
                :to-be-truthy)))))

(describe "resource-limit helper"
  (it "signals structured resource diagnostics"
    (let* ((span (test-point-span 13 14 15))
           (condition
             (handler-case
                 (yaml-kit::%signal-yaml-resource-limit
                  "depth" 3 4 :span span :context "parser context")
               (yaml-kit:yaml-resource-limit-error (condition) condition)))
           (diagnostic (yaml-kit:yaml-kit-error-diagnostic condition)))
      (expect (list (yaml-kit::yaml-resource-limit-error-limit-name condition)
                    (yaml-kit::yaml-resource-limit-error-limit condition)
                    (yaml-kit::yaml-resource-limit-error-actual condition)
                    (yaml-kit::yaml-resource-limit-error-context condition))
              :to-equal (list "depth" 3 4 "parser context"))
      (expect (list (cl-parser-kit:diagnostic-message diagnostic)
                    (cl-parser-kit:diagnostic-span diagnostic)
                    (cl-parser-kit:diagnostic-data diagnostic))
              :to-equal
              (list "YAML resource limit \"depth\" exceeded: limit 3, actual 4"
                    span
                    (list :limit-name "depth" :limit 3 :actual 4
                          :context "parser context")))
      (expect (condition-report condition)
              :to-equal (cl-parser-kit:diagnostic->string diagnostic)))))
