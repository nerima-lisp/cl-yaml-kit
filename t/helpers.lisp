;;;; t/helpers.lisp
(in-package #:cl-yaml-kit/test)

(defun test-timeout-ms ()
  (or (ignore-errors
        (parse-integer (uiop:getenv "CL_YAML_TEST_TIMEOUT_MS")))
      5000))

(defun run-tests ()
  (run-all :reporter :spec :pass-with-no-tests nil
           :timeout-ms (test-timeout-ms)))
