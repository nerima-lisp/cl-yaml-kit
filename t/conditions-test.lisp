;;;; t/conditions-test.lisp
(in-package #:cl-yaml-kit/test)

(defun condition-report (condition)
  (with-output-to-string (stream)
    (princ condition stream)))

(defmacro define-condition-contract-test (name form accessors expected)
  `(it ,(format nil "reports and exposes ~A" name)
     (let* ((condition ,form)
            (report (condition-report condition)))
       (expect (typep condition 'yaml-kit:yaml-kit-error) :to-be-truthy)
       (expect (every (lambda (value) (search value report)) ',expected)
               :to-be-truthy)
       ,@(mapcar (lambda (accessor)
                   `(expect (funcall #',accessor condition) :to-be-truthy))
                 accessors))))

(describe "condition contract"
  (it "reports the base message"
    (let ((condition (make-condition 'yaml-kit:yaml-kit-error :message "base")))
      (expect (typep condition 'error) :to-be-truthy)
      (expect (search "base" (condition-report condition)) :to-be-truthy)
      (expect (yaml-kit::yaml-kit-error-message condition) :to-equal "base")))
  (define-condition-contract-test yaml-parse-error
    (make-condition 'yaml-kit:yaml-parse-error :line 4 :column 5 :offset 6
                    :context "context" :message "detail")
    (yaml-kit:yaml-parse-error-line yaml-kit:yaml-parse-error-column
     yaml-kit:yaml-parse-error-offset yaml-kit:yaml-parse-error-context)
    ("line 4" "column 5" "offset 6" "context"))
  (define-condition-contract-test yaml-compose-error
    (make-condition 'yaml-kit:yaml-compose-error
                    :mark (yaml-kit:make-mark 7 8 9) :context "compose"
                    :message "detail")
    (yaml-kit::yaml-compose-error-mark yaml-kit::yaml-compose-error-context)
    ("line 7" "column 8" "offset 9" "compose"))
  (define-condition-contract-test yaml-emit-error
    (make-condition 'yaml-kit:yaml-emit-error
                    :mark (yaml-kit:make-mark 10 11 12) :context "emit"
                    :message "detail")
    (yaml-kit::yaml-emit-error-mark yaml-kit::yaml-emit-error-context)
    ("line 10" "column 11" "offset 12" "emit"))
  (define-condition-contract-test yaml-resource-limit-error
    (make-condition 'yaml-kit:yaml-resource-limit-error
                    :limit-name "depth" :limit 3 :actual 4
                    :mark (yaml-kit:make-mark 13 14 15) :context "resource"
                    :message "detail")
    (yaml-kit::yaml-resource-limit-error-limit-name
     yaml-kit::yaml-resource-limit-error-limit yaml-kit::yaml-resource-limit-error-actual
     yaml-kit::yaml-resource-limit-error-mark yaml-kit::yaml-resource-limit-error-context)
    ("line 13" "column 14" "offset 15" "resource")))
