;;;; t/helpers.lisp
(in-package #:cl-yaml-kit/test)

(defun test-text (text)
  "The scanner takes a simple character array, so hand it one."
  (make-array (length text) :element-type 'character :initial-contents text))

(defun test-timeout-ms ()
  (let ((value (uiop:getenv "CL_YAML_TEST_TIMEOUT_MS")))
    (if value
        (parse-integer value :junk-allowed nil)
        120000)))

(defun run-tests ()
  (setf *conformance-last-summaries* nil)
  (let ((unit-tests-passed
          (run-all :reporter :spec :pass-with-no-tests nil
                   :timeout-ms (test-timeout-ms)))
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
