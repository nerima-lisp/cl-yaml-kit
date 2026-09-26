;;;; Compile with SB-COVER instrumentation, then run the registered cl-weave tests.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (require :sb-cover))

(let* ((script (or *load-truename* *compile-file-truename*))
       (root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-directory-pathname script)))
       (report (or (uiop:getenv "COVERAGE_OUTPUT") "cl-yaml-kit.coverage"))
       (directory (or (uiop:getenv "COVERAGE_REPORT_DIRECTORY")
                      "cl-yaml-kit-coverage-report/")))
  (uiop:chdir root)
  (proclaim `(optimize (,(intern "STORE-COVERAGE-DATA" "SB-COVER") 3)))
  (asdf:operate 'asdf:compile-op "cl-yaml-kit/test" :force t)
  (sb-cover:reset-coverage)
  (let ((tests (uiop:symbol-call :cl-weave :list-tests
                                 :reporter :json
                                 :stream (make-broadcast-stream))))
    (format t "Loaded ~D tests for coverage.~%" (length tests))
    (when (zerop (length tests))
      (error "cl-yaml-kit loaded zero tests for coverage")))
  (unless (uiop:symbol-call :cl-weave :run-all
                            :reporter :spec :coverage t
                            :coverage-output report
                            :coverage-report-directory directory
                            :pass-with-no-tests nil)
    (uiop:quit 1)))
