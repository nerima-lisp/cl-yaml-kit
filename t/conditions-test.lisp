;;;; t/conditions-test.lisp
(in-package #:cl-yaml-kit/test)

(progn
  (defun test-diagnostic (message &optional span)
    (cl-parser-kit:make-diagnostic
     :kind :error :message message :span span))
  (defun condition-report (condition)
    (with-output-to-string (stream)
      (princ condition stream))))

(defmacro define-condition-contract-test (name form accessors expected)
  `(it ,(format nil "reports and exposes ~A" name)
     (let* ((condition ,form)
            (diagnostic (yaml-kit:yaml-kit-error-diagnostic condition))
            (report (condition-report condition)))
       (expect (typep condition 'yaml-kit:yaml-kit-error) :to-be-truthy)
       (expect (typep diagnostic 'cl-parser-kit:diagnostic) :to-be-truthy)
       (expect (stringp (cl-parser-kit:diagnostic-message diagnostic))
               :to-be-truthy)
       (expect report :to-equal
               (cl-parser-kit:diagnostic->string diagnostic))
       (expect (every (lambda (value) (search value report)) ',expected)
               :to-be-truthy)
       ,@(mapcar (lambda (accessor)
                   `(expect (funcall #',accessor condition) :to-be-truthy))
                 accessors))))

(describe "condition contract"
  (it "reports the base diagnostic message"
    (let* ((diagnostic (test-diagnostic "base"))
           (condition
             (make-condition 'yaml-kit:yaml-kit-error
                             :diagnostic diagnostic))
           (report (condition-report condition)))
      (expect (typep condition 'error) :to-be-truthy)
      (expect (eq (yaml-kit:yaml-kit-error-diagnostic condition)
                  diagnostic)
              :to-be-truthy)
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
  (it "keeps context and cause readers"
    (let ((compose
            (make-condition 'yaml-kit:yaml-compose-error
                            :diagnostic (test-diagnostic "compose")
                            :context "compose context" :cause "compose cause"))
          (emit
            (make-condition 'yaml-kit:yaml-emit-error
                            :diagnostic (test-diagnostic "emit")
                            :context "emit context" :cause "emit cause")))
      (expect (yaml-kit:yaml-compose-error-context compose)
              :to-equal "compose context")
      (expect (yaml-kit:yaml-compose-error-cause compose)
              :to-equal "compose cause")
      (expect (yaml-kit:yaml-emit-error-context emit)
              :to-equal "emit context")
      (expect (yaml-kit:yaml-emit-error-cause emit)
              :to-equal "emit cause")
      (expect (condition-report compose) :to-equal
              (cl-parser-kit:diagnostic->string
               (yaml-kit:yaml-kit-error-diagnostic compose)))
      (expect (condition-report emit) :to-equal
              (cl-parser-kit:diagnostic->string
               (yaml-kit:yaml-kit-error-diagnostic emit)))))
  (it "stringifies non-string compose error messages"
    (let ((condition
            (handler-case
                (yaml-kit::signal-yaml-compose-error :message :unknown)
              (yaml-kit:yaml-compose-error (condition) condition))))
      (expect (cl-parser-kit:diagnostic-message
               (yaml-kit:yaml-kit-error-diagnostic condition))
              :to-equal "UNKNOWN")))
  (it "signals errors through the compose and emit helpers"
    (let ((compose
            (handler-case
                (yaml-kit::signal-yaml-compose-error
                 :message "compose helper" :cause "cause")
              (yaml-kit:yaml-compose-error (condition) condition)))
          (emit
            (handler-case
                (yaml-kit::signal-yaml-emit-error
                 :diagnostic (test-diagnostic "emit helper")
                 :context "emit context")
              (yaml-kit:yaml-emit-error (condition) condition))))
      (expect (cl-parser-kit:diagnostic-message
               (yaml-kit:yaml-kit-error-diagnostic compose))
              :to-equal "compose helper")
      (expect (yaml-kit:yaml-compose-error-cause compose) :to-equal "cause")
      (expect (cl-parser-kit:diagnostic-message
               (yaml-kit:yaml-kit-error-diagnostic emit))
              :to-equal "emit helper")
      (expect (yaml-kit:yaml-emit-error-context emit)
              :to-equal "emit context")))
  (define-condition-contract-test yaml-parse-error
    (make-condition 'yaml-kit:yaml-parse-error
                    :diagnostic (test-diagnostic
                                 "parse detail" (test-point-span 4 5 6))
                    :context "context")
    (yaml-kit:yaml-parse-error-context)
    ("parse detail"))
  (define-condition-contract-test yaml-compose-error
    (make-condition 'yaml-kit:yaml-compose-error
                    :diagnostic (test-diagnostic
                                 "compose detail" (test-point-span 7 8 9))
                    :context "compose" :cause "cause")
    (yaml-kit:yaml-compose-error-context yaml-kit:yaml-compose-error-cause)
    ("compose detail"))
  (define-condition-contract-test yaml-emit-error
    (make-condition 'yaml-kit:yaml-emit-error
                    :diagnostic (test-diagnostic
                                 "emit detail" (test-point-span 10 11 12))
                    :context "emit" :cause "cause")
    (yaml-kit:yaml-emit-error-context yaml-kit:yaml-emit-error-cause)
    ("emit detail"))
  (define-condition-contract-test yaml-resource-limit-error
    (make-condition 'yaml-kit:yaml-resource-limit-error
                    :diagnostic (test-diagnostic
                                 "resource detail" (test-point-span 13 14 15))
                    :limit-name "depth" :limit 3 :actual 4 :context "resource")
    (yaml-kit::yaml-resource-limit-error-limit-name
     yaml-kit::yaml-resource-limit-error-limit
     yaml-kit::yaml-resource-limit-error-actual
     yaml-kit::yaml-resource-limit-error-context)
    ("resource detail"))
  (define-condition-contract-test yaml-format-edit-path-error
    (make-condition 'yaml-kit:yaml-format-edit-path-error
                    :diagnostic (test-diagnostic "path detail")
                    :path '("a") :document 1 :operation :delete)
    (yaml-kit:yaml-format-edit-path-error-path
     yaml-kit:yaml-format-edit-path-error-document
     yaml-kit:yaml-format-edit-path-error-operation)
    ("path detail"))
  (define-condition-contract-test yaml-format-edit-structure-error
    (make-condition 'yaml-kit:yaml-format-edit-structure-error
                    :diagnostic (test-diagnostic "structure detail")
                    :path '("a" 0))
    (yaml-kit:yaml-format-edit-structure-error-path)
    ("structure detail"))
  (define-condition-contract-test yaml-format-edit-anchor-error
    (make-condition 'yaml-kit:yaml-format-edit-anchor-error
                    :diagnostic (test-diagnostic "anchor detail")
                    :path '("a") :anchor "base")
    (yaml-kit:yaml-format-edit-anchor-error-path
     yaml-kit:yaml-format-edit-anchor-error-anchor)
    ("anchor detail")))

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
