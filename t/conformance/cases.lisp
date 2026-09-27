(in-package #:cl-yaml-kit/test)

(defun register-conformance-case-tests ()
  (let ((cases (conformance-cases))
        (exclusions (conformance-read-exclusions)))
    (dolist (stage *conformance-stage-names*)
      (dolist (case cases)
        (multiple-value-bind (applicable passedp condition)
            (conformance-stage-result case stage)
          (declare (ignore passedp condition))
          (when applicable
            (let* ((id (conformance-case-id case))
                   (name (format nil "~A case ~A" stage id))
                   (excluded (conformance-excluded-p case stage exclusions)))
              (eval
               `(it ,name
                  ,@(when excluded '((:skip-reason "listed conformance exclusion")))
                  (multiple-value-bind (case-applicable case-passedp)
                      (conformance-stage-result
                       (find ,id (conformance-cases)
                             :key #'conformance-case-id :test #'string=)
                       ,stage)
                    (expect (and case-applicable
                                 (or (eq case-passedp :skipped) case-passedp))
                            :to-be-truthy)))))))))))

(register-conformance-case-tests)
