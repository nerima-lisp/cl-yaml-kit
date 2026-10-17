;;;; t/coverage-gate-test.lisp
(in-package #:cl-yaml-kit/test)

(defparameter *gate-source*
  (format nil "(in-package #:cl-user)~%(defun gated (x &key (y 1))~%  (if x y 0))~%"))

(defparameter *gate-rules* '((:head "in-package" :reason "compile time")
                             (:head "defun" :reason "test-only rule")))

(defparameter *strict-allowance* '(:expression 0 :branch 0))

(defun gate-location (start end)
  (list :kind :expression :start start :end end
        :line 1 :column (1+ start) :end-line 1 :end-column (1+ end)))

(defparameter *in-package-miss* (gate-location 0 22))
(defparameter *defun-miss* (gate-location 23 64))
(defparameter *default-miss* (gate-location 44 49))

(defun gate-statistics (expression-covered branch-covered &rest locations)
  (list :pathname #p"/virtual/gated.lisp"
        :expression-covered expression-covered :expression-total 10
        :branch-covered branch-covered :branch-total 10
        :uncovered-forms locations))

(defun gate-verdict (statistics)
  (cl-yaml-kit/coverage-gate:file-verdict statistics *gate-source* *gate-rules*))

(defun gate-status (verdicts &key (rules (list (first *gate-rules*)))
                                  (allowance *strict-allowance*))
  (cl-yaml-kit/coverage-gate:gate-exit-status verdicts rules allowance))

(defun gate-report (verdicts &key (rules (list (first *gate-rules*))))
  (with-output-to-string (stream)
    (cl-yaml-kit/coverage-gate:write-gate-report verdicts rules *strict-allowance* stream)))

(describe "coverage gate"
  (it "reads top-level form heads and spans"
    (expect (cl-yaml-kit/coverage-gate:top-level-forms *gate-source*)
            :to-equal '((:head "in-package" :start 0 :end 22)
                        (:head "defun" :start 23 :end 64))))

  (it "fails a file at 50% expression and 10% branch coverage"
    (let ((verdicts (list (gate-verdict (gate-statistics 5 1 *in-package-miss*)))))
      (expect (mapcar (lambda (kind) (getf (first verdicts) kind))
                      '(:expression-shortfall :branch-shortfall))
              :to-equal '(4 9))
      (expect (gate-status verdicts) :to-equal 1)
      (expect (gate-report verdicts) :to-contain "coverage gate: FAIL")))

  (it "passes when the only miss is a whole excluded top-level form"
    (let ((verdicts (list (gate-verdict (gate-statistics 9 10 *in-package-miss*)))))
      (expect (gate-status verdicts) :to-equal 0)
      (expect (gate-report verdicts) :to-contain "excused in-package gated.lisp:1")
      (expect (gate-report verdicts) :to-contain "coverage gate: PASS")))

  (it "excuses forms nested in an excluded form that never ran as a whole"
    (let ((verdict (gate-verdict (gate-statistics 8 10 *defun-miss* *default-miss*))))
      (expect (getf verdict :expression-shortfall) :to-equal 0)
      (expect (getf verdict :unexcused) :to-equal nil)))

  (it "does not excuse a form nested inside an excluded head that ran"
    (let ((verdict (gate-verdict (gate-statistics 9 10 *default-miss*))))
      (expect (getf verdict :expression-shortfall) :to-equal 1)
      (expect (getf verdict :unexcused) :to-equal (list *default-miss*))))
  (it "never excuses a branch shortfall inside an excluded form"
    (let ((branch-miss (gate-location 30 35)))
      (setf (getf branch-miss :kind) :branch)
      (let ((verdicts
              (list (gate-verdict
                     (gate-statistics 9 9 *defun-miss* branch-miss)))))
        (expect (getf (first verdicts) :expression-shortfall) :to-equal 0)
        (expect (getf (first verdicts) :branch-shortfall) :to-equal 1)
        (expect (getf (first verdicts) :unexcused) :to-equal (list branch-miss))
        (expect (gate-status verdicts :rules (list (second *gate-rules*)))
                :to-equal 1))))

  (it "tolerates a shortfall within the allowance and fails beyond it"
    (let ((verdicts (list (gate-verdict (gate-statistics 8 10 *in-package-miss*
                                                         *default-miss*)))))
      (expect (gate-status verdicts :allowance '(:expression 1 :branch 0)) :to-equal 0)
      (expect (gate-status verdicts) :to-equal 1)))

  (it "fails on an exclusion rule that excused nothing"
    (let ((verdicts (list (gate-verdict (gate-statistics 9 10 *in-package-miss*)))))
      (expect (gate-status verdicts :rules *gate-rules*) :to-equal 1)
      (expect (gate-report verdicts :rules *gate-rules*)
              :to-contain "stale exclusion: defun excused nothing")))

  (it "fails when no file was measured"
    (expect (gate-status nil) :to-equal 1))

  (it "loads the repository exclusions and allowance"
    (flet ((data (name)
             (asdf:system-relative-pathname "cl-yaml-kit" (format nil "scripts/~A" name))))
      (expect (cl-yaml-kit/coverage-gate:read-exclusion-rules
               (data "coverage-exclusions.sexp"))
              :to-satisfy #'consp)
      (expect (cl-yaml-kit/coverage-gate:read-allowance (data "coverage-allowance.sexp"))
              :to-satisfy #'consp))))
