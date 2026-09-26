;;;; scripts/run-coverage.lisp
(require :asdf)
(require :sb-cover)
(asdf:load-system "cl-yaml-kit/test")
(sb-cover:reset-coverage)
(unless (uiop:symbol-call :cl-yaml-kit/test :run-tests)
  (uiop:quit 1))
(sb-cover:report :base-directory (truename ".")
                 :report-directory "cl-yaml-kit-coverage-report/")
