;;;; Compile with SB-COVER instrumentation, then run the registered cl-weave tests.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :asdf)
  (require :sb-cover))

(let* ((script (or *load-truename* *compile-file-truename*))
       (root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-directory-pathname script)))
       (report (or (uiop:getenv "COVERAGE_OUTPUT") "cl-yaml-kit.coverage"))
       (directory (or (uiop:getenv "COVERAGE_REPORT_DIRECTORY")
                      "cl-yaml-kit-coverage-report/"))
       (name-filter (or (uiop:getenv "CL_YAML_COVERAGE_FILTER") "loader >")))
  (uiop:chdir root)
  (ensure-directories-exist (uiop:ensure-directory-pathname directory))
  (proclaim `(optimize (,(intern "STORE-COVERAGE-DATA" "SB-COVER") 3)))
  ;; Register the local ASDF definition before referring to the test system.
  ;; This keeps the script usable from a clean SBCL image.
  (asdf:load-asd (merge-pathnames "cl-yaml-kit.asd" root))
  (asdf:load-system "cl-yaml-kit/test")
  (asdf:operate 'asdf:compile-op "cl-yaml-kit" :force t)
  (asdf:operate 'asdf:compile-op "cl-yaml-kit/test" :force t)
  (sb-cover:reset-coverage)
  (let ((tests (uiop:symbol-call :cl-weave :list-tests
                                 :reporter :json
                                 :name-filter name-filter
                                 :stream (make-broadcast-stream))))
    (format t "Loaded ~D tests for coverage.~%" (length tests))
    (when (zerop (length tests))
      (error "cl-yaml-kit loaded zero tests for coverage")))
  (unless (uiop:symbol-call :cl-weave :run-all
                            :reporter :spec :coverage t
                            :name-filter name-filter
                            :coverage-output report
                            :coverage-report-directory directory
                            :pass-with-no-tests nil)
    (uiop:quit 1)))
