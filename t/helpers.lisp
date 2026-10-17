;;;; t/helpers.lisp
(in-package #:cl-yaml-kit/test)

(defparameter *default-test-timeout-ms* 120000)

(defun unit-run-options ()
  "cl-weave's CL_WEAVE_* settings, with a per-test timeout and an empty run failing."
  (let ((options (cl-weave/cli:run-all-options-from-environment)))
    (setf (getf options :pass-with-no-tests) nil)
    (unless (getf options :timeout-ms)
      (setf (getf options :timeout-ms) *default-test-timeout-ms*))
    options))

(defun run-tests ()
  (setf *conformance-last-summaries* nil)
  (let ((unit-tests-passed
          (apply #'run-all (unit-run-options)))
        (conformance-passed nil))
    (unless *conformance-last-summaries*
      (multiple-value-bind (summaries passedp)
          (conformance-run-report)
        (declare (ignore summaries))
        (setf conformance-passed passedp)))
    (when *conformance-last-summaries*
      (setf conformance-passed
            (every (lambda (summary)
                    (and (zerop (getf summary :failed))
                         (zerop (getf summary :drift))))
                   *conformance-last-summaries*)))
    (and unit-tests-passed conformance-passed)))
