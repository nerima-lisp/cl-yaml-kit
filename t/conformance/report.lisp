(in-package #:cl-yaml-kit/test)

(defun conformance-run-report ()
  (let* ((cases (conformance-cases))
         (exclusions (conformance-read-exclusions))
         (summaries (mapcar (lambda (stage)
                              (conformance-stage-summary cases stage exclusions))
                            *conformance-stage-names*)))
    (dolist (summary summaries)
      (format t "~&conformance ~A: applicable=~D passed=~D failed=~D excluded=~D drift=~D~%"
              (getf summary :stage) (getf summary :total)
              (getf summary :passed) (getf summary :failed)
              (getf summary :excluded) (getf summary :drift)))
    (setf *conformance-last-summaries* summaries)
    (values summaries
            (every (lambda (summary)
                    (and (zerop (getf summary :failed))
                         (zerop (getf summary :drift))))
                   summaries))))
