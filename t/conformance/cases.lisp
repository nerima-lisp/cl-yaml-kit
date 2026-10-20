(in-package #:cl-yaml-kit/test)

(defun register-conformance-case-tests ()
  (let ((cases (conformance-cases)))
    (eval
     `(it-each
          ,(loop for stage in *conformance-stage-names*
                 append
                 (loop for case in cases
                       when (nth-value 0 (conformance-stage-result case stage))
                         collect (list stage (conformance-case-id case))))
        "~A case ~S"
        (stage case-id)
        (multiple-value-bind (case-applicable case-passedp)
            (conformance-stage-result
             (find case-id (conformance-cases)
                   :key #'conformance-case-id :test #'string=)
             stage)
          (expect (and case-applicable
                       (or (eq case-passedp :skipped) case-passedp))
                  :to-be-truthy))))
    nil))

(register-conformance-case-tests)
