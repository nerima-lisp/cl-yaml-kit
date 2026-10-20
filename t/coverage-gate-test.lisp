;;;; t/coverage-gate-test.lisp
(in-package #:cl-yaml-kit/test)
(eval-when (:compile-toplevel :load-toplevel :execute) (require :sb-posix))

(defparameter *gate-source*
  (format nil "(in-package #:cl-user)~%(defun gated (x &key (y 1))~%  (if x y 0))~%"))

(defparameter *gate-rules* '((:head "in-package" :reason "compile time")
                             (:head "defun" :reason "test-only rule")))

(defun gate-location (start end &optional (kind :expression))
  (list :kind kind :start start :end end
        :line 1 :column (1+ start) :end-line 1 :end-column (1+ end)))

(defparameter *in-package-miss* (gate-location 0 22))
(defparameter *defun-miss* (gate-location 23 64))
(defparameter *default-miss* (gate-location 44 49))

(defun gate-statistics (expression-covered expression-total
                         branch-covered branch-total &rest locations)
  (list :pathname #p"/virtual/gated.lisp"
        :expression-covered expression-covered :expression-total expression-total
        :branch-covered branch-covered :branch-total branch-total
        :uncovered-forms locations))

(defun gate-verdict (statistics &optional (rules nil))
  (cl-yaml-kit/coverage-gate:file-verdict statistics *gate-source* rules))

(defun gate-status (verdicts &optional (rules nil))
  (cl-yaml-kit/coverage-gate:gate-exit-status verdicts rules))

(defun gate-report (verdicts &optional (rules nil))
  (with-output-to-string (stream)
    (cl-yaml-kit/coverage-gate:write-gate-report verdicts rules stream)))

(defun run-coverage-reproduction-process
    (label &key (timeout 180) form systems)
  (let* ((root (asdf:system-source-directory "cl-yaml-kit"))
         (child-form
           (if form
               form
               `(load (merge-pathnames
                         (format nil "scripts/coverage-reproductions/~A" ,label)
                         ,root))))
         (result
           (cl-weave:run-isolated
            child-form
            :systems systems
            :package "CL-USER"
            :timeout timeout))
         (output (cl-weave:isolated-result-stdout result))
         (error-output (cl-weave:isolated-result-stderr result))
         (status (cl-weave:isolated-result-status result))
         (exit-code (cl-weave:isolated-result-exit-code result)))
    (case status
      (:timeout
       (error "Subprocess ~A timed out after ~A seconds.~%stdout:~%~A~%stderr:~%~A"
              label timeout output error-output))
      ((:pass :fail) nil)
      (otherwise
       (error "Subprocess ~A returned unexpected status ~S.~%stdout:~%~A~%stderr:~%~A"
              label status output error-output)))
    (values (concatenate 'string output error-output) exit-code status)))

(describe "coverage gate"
  (it "reads top-level form heads and spans"
    (expect (cl-yaml-kit/coverage-gate:top-level-forms *gate-source*)
            :to-equal '((:head "in-package" :start 0 :end 22)
                        (:head "defun" :start 23 :end 64))))

  (it "passes both aggregate thresholds at exactly 90 percent"
    (let ((verdict (gate-verdict (gate-statistics 9 10 9 10))))
      (expect (gate-status (list verdict)) :to-equal 0)
      (expect (gate-report (list verdict))
              :to-contain "aggregate expression: 9/10 (90% threshold) PASS")
      (expect (gate-report (list verdict))
              :to-contain "aggregate branch: 9/10 (90% threshold) PASS")))
  (it "aggregates separate files before applying coverage thresholds"
    (let* ((under-covered-statistics (gate-statistics 0 1 0 1))
           (fully-covered-statistics (gate-statistics 9 9 9 9))
           (under-covered-verdict (gate-verdict under-covered-statistics))
           (fully-covered-verdict
             (progn
               (setf (getf fully-covered-statistics :pathname)
                     #p"/virtual/other.lisp")
               (gate-verdict fully-covered-statistics)))
           (verdicts (list under-covered-verdict fully-covered-verdict)))
      (expect (gate-status verdicts) :to-equal 0)
      (expect (gate-report verdicts)
              :to-contain "aggregate expression: 9/10 (90% threshold) PASS")
      (expect (gate-report verdicts)
              :to-contain "aggregate branch: 9/10 (90% threshold) PASS")))

  (it "fails when aggregate expression coverage is below 90 percent"
    (let ((verdict (gate-verdict (gate-statistics 8 10 9 10))))
      (expect (gate-status (list verdict)) :to-equal 1)
      (expect (gate-report (list verdict))
              :to-contain "aggregate expression: 8/10 (90% threshold) FAIL")
      (expect (gate-report (list verdict))
              :to-contain "aggregate branch: 9/10 (90% threshold) PASS")))

  (it "fails when aggregate branch coverage is below 90 percent"
    (let ((verdict (gate-verdict (gate-statistics 9 10 8 10))))
      (expect (gate-status (list verdict)) :to-equal 1)
      (expect (gate-report (list verdict))
              :to-contain "aggregate expression: 9/10 (90% threshold) PASS")
      (expect (gate-report (list verdict))
              :to-contain "aggregate branch: 8/10 (90% threshold) FAIL")))

  (it "removes an excluded form from the expression denominator without crediting it"
    (let* ((rules (list (second *gate-rules*)))
           (verdict (gate-verdict
                     (gate-statistics 8 10 10 10 *defun-miss*) rules)))
      (expect (list (getf verdict :expression-covered)
                    (getf verdict :expression-total)
                    (getf verdict :expression-shortfall))
              :to-equal (list 8 9 1))
      (expect (gate-status (list verdict) rules) :to-equal 1)
      (expect (gate-report (list verdict) rules)
              :to-contain "aggregate expression: 8/9 (90% threshold) FAIL")))

  (it "excuses only an exact excluded top-level expression"
    (let* ((rules (list (first *gate-rules*)))
           (verdict (gate-verdict
                     (gate-statistics 8 10 9 10 *in-package-miss* *default-miss*)
                     rules)))
      (expect (getf verdict :expression-shortfall) :to-equal 1)
      (expect (getf verdict :unexcused) :to-equal (list *default-miss*))))

  (it "leaves nested expressions and branches uncovered under excluded forms"
    (let* ((source
             (format nil "~A~%~A~%~A~%~A~%~A~%~A~%"
                     "(in-package #:cl-user)"
                     "(defparameter *gated* (progn"
                     "  (if *never* (error \"parameter initform\"))"
                     "  (lambda () (if *never* (error \"lambda body\")))))"
                     "(defstruct gated-structure"
                     "  (value (if *never* (error \"structure initform\"))))"))
           (forms (cl-yaml-kit/coverage-gate:top-level-forms source))
           (parameter-form (find "defparameter" forms
                                 :key (lambda (form) (getf form :head))
                                 :test #'equal))
           (structure-form (find "defstruct" forms
                                 :key (lambda (form) (getf form :head))
                                 :test #'equal)))
      (labels ((span-for (text kind)
                 (let ((start (search text source)))
                   (unless start
                     (error "Missing nested fixture text ~S." text))
                   (let ((location (gate-location start (+ start (length text)))))
                     (setf (getf location :kind) kind)
                     location))))
        (let* ((parameter-miss
                 (gate-location (getf parameter-form :start)
                                (getf parameter-form :end)))
               (initform-miss (span-for "(error \"parameter initform\")" :expression))
               (lambda-miss (span-for "(error \"lambda body\")" :expression))
               (structure-miss
                 (gate-location (getf structure-form :start)
                                (getf structure-form :end)))
               (slot-initform-miss
                 (span-for "(error \"structure initform\")" :expression))
               (branch-miss
                 (span-for "(if *never* (error \"parameter initform\"))" :branch))
               (statistics
                 (list :pathname #p"/virtual/nested-runtime.lisp"
                       :expression-covered 5 :expression-total 10
                       :branch-covered 1 :branch-total 2
                       :uncovered-forms
                       (list parameter-miss initform-miss lambda-miss
                             structure-miss slot-initform-miss branch-miss)))
               (rules (list '(:head "defparameter" :reason "initform fixture")
                            '(:head "defstruct" :reason "slot initform fixture")))
               (verdict (cl-yaml-kit/coverage-gate:file-verdict
                         statistics source rules)))
          (expect (list (getf verdict :expression-total)
                        (getf verdict :expression-shortfall)
                        (getf verdict :branch-shortfall))
                  :to-equal (list 8 3 1))
          (expect (getf verdict :unexcused)
                  :to-equal
                  (list initform-miss lambda-miss slot-initform-miss branch-miss))))))

  (it "never excuses a branch miss inside an excluded top-level form"
    (let* ((rules (list (second *gate-rules*)))
           (branch-miss (gate-location 53 63 :branch))
           (verdict (gate-verdict
                     (gate-statistics 9 10 8 10 *defun-miss* branch-miss)
                     rules)))
      (expect (<= (getf *defun-miss* :start)
                  (getf branch-miss :start)
                  (getf branch-miss :end)
                  (getf *defun-miss* :end))
              :to-equal t)
      (expect (list (getf verdict :expression-total)
                    (getf verdict :expression-shortfall)
                    (getf verdict :branch-shortfall))
              :to-equal (list 9 0 2))
      (expect (list (getf verdict :unexcused)
                    (gate-status (list verdict) rules))
              :to-equal (list (list branch-miss) 1))))

  (it "fails when an exclusion rule excused nothing"
    (let* ((verdict (gate-verdict (gate-statistics 9 10 9 10)))
           (rules (list (second *gate-rules*))))
      (expect (gate-status (list verdict) rules) :to-equal 1)
      (expect (gate-report (list verdict) rules)
              :to-contain "stale exclusion: defun excused nothing")))

  (it "fails when no file was measured"
    (expect (gate-status nil) :to-equal 1))

  (it "fails when an expected source file was not measured"
    (let* ((pathname (asdf:system-relative-pathname
                      "cl-yaml-kit" "t/coverage-gate-test.lisp"))
           (statistics (gate-statistics 9 10 9 10)))
      (setf (getf statistics :pathname) pathname)
      (expect (lambda ()
                (cl-yaml-kit/coverage-gate:gate-verdicts
                 (list statistics) nil
                 :expected-pathnames (list pathname #p"/virtual/missing.lisp")))
              :to-throw
              (lambda (condition)
                (search "missing:" (princ-to-string condition))))))

  (it "fails when a source file was measured more than once"
    (let* ((pathname (asdf:system-relative-pathname
                      "cl-yaml-kit" "t/coverage-gate-test.lisp"))
           (statistics (gate-statistics 9 10 9 10))
           (duplicate (copy-list statistics)))
      (setf (getf statistics :pathname) pathname
            (getf duplicate :pathname) pathname)
      (expect (lambda ()
                (cl-yaml-kit/coverage-gate:gate-verdicts
                 (list statistics duplicate) nil
                 :expected-pathnames (list pathname)))
              :to-throw
              (lambda (condition)
                (search "Duplicate measured source files:"
                        (princ-to-string condition))))))

  (it "rejects fake 50 percent expression and 10 percent branch coverage"
    (multiple-value-bind (output exit-code status)
        (run-coverage-reproduction-process "fake-gate.lisp")
      (expect (list status exit-code) :to-equal (list :fail 1))
      (expect (every (lambda (fragment) (search fragment output))
                     (list "fake coverage: expression 50%, branch 10%"
                           "aggregate expression:"
                           "aggregate branch:"
                           "coverage gate: FAIL"))
              :to-be-truthy)))

  (it "keeps nested runtime expressions uncovered when a synthetic whole-form marker is excluded"
    (multiple-value-bind (output exit-code status)
        (run-coverage-reproduction-process "run.lisp" :systems (list "sb-cover" "cl-weave"))
      (expect (list status exit-code) :to-equal (list :pass 0))
      (expect (every (lambda (fragment) (search fragment output))
                     (list "synthetic whole-form exclusion leaves nested runtime expressions uncovered: PASS"
                           "coverage exclusion regression: PASS"))
              :to-be-truthy)))

  (it "terminates an inline child after timeout"
    (let* ((pid-file
             (loop for candidate = (merge-pathnames
                                    (format nil "cl-yaml-kit-timeout-~D.pid"
                                            (random 1000000000))
                                    (uiop:temporary-directory))
                   unless (probe-file candidate)
                     return candidate))
           (child-form
             `(progn
                (with-open-file (cl-user::stream ,(namestring pid-file)
                                 :direction :output :if-exists :supersede)
                  (write-line
                   (write-to-string
                    (funcall
                     (symbol-function (find-symbol "UNIX-GETPID" "SB-UNIX"))))
                   cl-user::stream)
                  (finish-output cl-user::stream))
                (write-line "coverage-timeout-stdout" *standard-output*)
                (finish-output *standard-output*)
                (write-line "coverage-timeout-stderr" *error-output*)
                (finish-output *error-output*)
                (sleep 30))))
      (unwind-protect
           (progn
             (expect (lambda ()
                       (run-coverage-reproduction-process
                        "inline timeout form" :timeout 5.0
                        :form child-form :systems nil))
                     :to-throw
                     (lambda (condition)
                       (let* ((message (princ-to-string condition))
                              (stdout-start (search "stdout:" message))
                              (stderr-start (search "stderr:" message))
                              (stdout-marker
                                (and stdout-start stderr-start
                                     (search "coverage-timeout-stdout" message
                                             :start2 (+ stdout-start (length "stdout:"))
                                             :end2 stderr-start)))
                              (stderr-marker
                                (and stderr-start
                                     (search "coverage-timeout-stderr" message
                                             :start2 (+ stderr-start (length "stderr:"))))))
                         (and (search "timed out after" message)
                              stdout-marker
                              stderr-marker))))
             (let ((pid
                     (with-open-file (stream pid-file)
                       (parse-integer (read-line stream)))))
               (expect (lambda () (sb-posix:kill pid 0))
                       :to-throw
                       (lambda (condition)
                         (and (typep condition 'sb-posix:syscall-error)
                              (= (sb-posix:syscall-errno condition)
                                 sb-posix:esrch))))))
        (when (probe-file pid-file)
          (delete-file pid-file)))))

  (it "loads the repository exclusions"
    (expect (cl-yaml-kit/coverage-gate:read-exclusion-rules
             (asdf:system-relative-pathname
              "cl-yaml-kit" "scripts/coverage-exclusions.sexp"))
            :to-equal nil)))
