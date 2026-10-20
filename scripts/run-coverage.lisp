;;;; scripts/run-coverage.lisp
;;;; Run the suite with src/ instrumented, then apply the coverage gate.
(require :asdf)
(require :sb-cover)

(defun checkout-root ()
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname
    (or *load-truename* (error "Unable to determine the coverage script location")))))

(defun environment-pathname (variable default)
  (let ((value (uiop:getenv variable)))
    (if (and value (plusp (length value))) (pathname value) default)))

(defun run-suite ()
  (handler-case (uiop:symbol-call :cl-yaml-kit/test :run-tests)
    (serious-condition (condition)
      (format *error-output* "~&cl-yaml-kit test run failed: ~A~%" condition)
      nil)))

(let* ((root (checkout-root))
       (source-directory (merge-pathnames "src/" root))
       (source-files (directory (merge-pathnames "*.lisp" source-directory)))
       (output-directory (merge-pathnames "coverage/" root))
       (data-file (environment-pathname "COVERAGE_OUTPUT"
                                        (merge-pathnames "cl-yaml-kit.coverage" output-directory)))
       (report-directory (uiop:ensure-directory-pathname
                          (environment-pathname "COVERAGE_REPORT_DIRECTORY"
                                                (merge-pathnames "report/" output-directory))))
       (cache-directory (uiop:ensure-directory-pathname
                         (environment-pathname "COVERAGE_ASDF_CACHE"
                                               (merge-pathnames "asdf-cache/" output-directory)))))
  (uiop:chdir root)
  (unless (uiop:getenv "YAML_TEST_SUITE")
    (error "YAML_TEST_SUITE must name the yaml-test-suite fixture directory."))
  (ensure-directories-exist data-file)
  (ensure-directories-exist cache-directory)
  (let ((asdf:*user-cache* cache-directory))
    (asdf:clear-output-translations)
    (asdf:initialize-output-translations)
    (asdf:load-asd (merge-pathnames "cl-yaml-kit.asd" root))
    (asdf:load-system "cl-weave")
    (asdf:load-system "cl-yaml-kit/coverage-gate")
    (uiop:symbol-call :cl-weave :reset-coverage)
    (proclaim '(optimize (sb-cover:store-coverage-data 3)))
    (asdf:operate 'asdf:compile-op "cl-yaml-kit" :force t)
    (proclaim '(optimize (sb-cover:store-coverage-data 0)))
    (asdf:load-system "cl-yaml-kit/test")
    (let* ((passed (run-suite))
           (statistics (uiop:symbol-call :cl-weave :coverage-file-statistics
                                         :include-pathnames (list source-directory)))
           (rules (uiop:symbol-call :cl-yaml-kit/coverage-gate :read-exclusion-rules
                                    (merge-pathnames "scripts/coverage-exclusions.sexp" root)))
           (verdicts (uiop:symbol-call :cl-yaml-kit/coverage-gate :gate-verdicts
                                       statistics rules :expected-pathnames source-files)))
      (uiop:symbol-call :cl-weave :save-coverage data-file)
      (uiop:symbol-call :cl-weave :save-coverage-report report-directory
                        :include-pathnames (list source-directory))
      (uiop:symbol-call :cl-yaml-kit/coverage-gate :write-gate-report
                        verdicts rules *standard-output*)
      (uiop:quit (if (and passed
                          (zerop (uiop:symbol-call :cl-yaml-kit/coverage-gate
                                                   :gate-exit-status verdicts rules)))
                     0
                     1)))))
