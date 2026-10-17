;;;; t/coverage-gate-test.lisp
(in-package #:cl-yaml-kit/test)

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

(defun gate-statistics (expression-covered branch-covered &rest locations)
  (list :pathname #p"/virtual/gated.lisp"
        :expression-covered expression-covered :expression-total 10
        :branch-covered branch-covered :branch-total 10
        :uncovered-forms locations))

(defun gate-verdict (statistics)
  (cl-yaml-kit/coverage-gate:file-verdict statistics *gate-source* *gate-rules*))

(defun gate-status (verdicts &key (rules (list (first *gate-rules*)))
                                  (allowance nil))
  (cl-yaml-kit/coverage-gate:gate-exit-status verdicts rules allowance))

(defun gate-report (verdicts &key (rules (list (first *gate-rules*)))
                                  (allowance nil))
  (with-output-to-string (stream)
    (cl-yaml-kit/coverage-gate:write-gate-report verdicts rules allowance stream)))

(defun run-coverage-reproduction-process (filename)
  (let* ((root (asdf:system-source-directory "cl-yaml-kit"))
         (script (merge-pathnames
                  (format nil "scripts/coverage-reproductions/~A" filename)
                  root)))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program
         (list "sbcl" "--noinform" "--disable-debugger"
              "--eval" "(require :asdf)"
              "--eval" "(asdf:load-system \"cl-yaml-kit\")"
              "--load" (namestring (merge-pathnames "scripts/coverage-gate.lisp" root))
              "--script" (namestring script))
         :directory root :output :string :error-output :string
         :ignore-error-status t :timeout 180)
      (values (concatenate 'string output error-output) exit-code))))

(describe "coverage gate"
  (it "reads top-level form heads and spans"
    (expect (cl-yaml-kit/coverage-gate:top-level-forms *gate-source*)
            :to-equal '((:head "in-package" :start 0 :end 22)
                        (:head "defun" :start 23 :end 64))))

  (it "requires each file to match both exact allowance counts"
    (let* ((first-verdict
             (gate-verdict
              (gate-statistics 9 10 *in-package-miss*)))
           (second-statistics
             (gate-statistics 8 9 *in-package-miss*))
           (second-verdict
             (progn
               (setf (getf second-statistics :pathname) #p"/virtual/other.lisp")
               (gate-verdict second-statistics)))
           (allowance
             (list (list :file (getf first-verdict :file) :expression 0 :branch 0)
                   (list :file (getf second-verdict :file) :expression 1 :branch 1)))
           (swapped
             (list (list :file (getf first-verdict :file) :expression 1 :branch 1)
                   (list :file (getf second-verdict :file) :expression 0 :branch 0))))
      (expect (gate-status (list first-verdict second-verdict)
                           :allowance allowance)
              :to-equal 0)
      (expect (gate-status (list first-verdict second-verdict)
                           :allowance swapped)
              :to-equal 1)
      (expect (gate-report (list first-verdict second-verdict)
                           :allowance swapped)
              :to-contain "shortfall changed for")))

  (it "excuses only the exact excluded top-level expression"
    (let* ((verdict (gate-verdict
                     (gate-statistics 8 10 *defun-miss* *default-miss*)))
           (allowance
             (list (list :file (getf verdict :file) :expression 1 :branch 0))))
      (expect (getf verdict :expression-shortfall) :to-equal 1)
      (expect (getf verdict :unexcused) :to-equal (list *default-miss*))
      (expect (gate-status (list verdict) :rules (list (second *gate-rules*))
                           :allowance allowance)
              :to-equal 0)))

  (it "leaves uncovered initform and lambda expressions under excluded forms"
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
                   (let ((location
                           (gate-location start (+ start (length text)))))
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
               (branch-miss (span-for "(if *never* (error \"parameter initform\"))"
                                      :branch))
               (statistics
                 (list :pathname #p"/virtual/nested-runtime.lisp"
                       :expression-covered 5 :expression-total 10
                       :branch-covered 1 :branch-total 2
                       :uncovered-forms
                       (list parameter-miss initform-miss lambda-miss
                             structure-miss slot-initform-miss branch-miss)))
               (rules (list (first *gate-rules*)
                            '(:head "defparameter" :reason "initform fixture")
                            '(:head "defstruct" :reason "slot initform fixture")))
               (verdict
                 (cl-yaml-kit/coverage-gate:file-verdict statistics source rules)))
          (expect (getf verdict :expression-shortfall) :to-equal 3)
          (expect (getf verdict :branch-shortfall) :to-equal 1)
          (expect (getf verdict :unexcused)
                  :to-equal
                  (list initform-miss lambda-miss slot-initform-miss branch-miss))))))

  (it "never excuses a branch miss inside an excluded top-level form"
    (let* ((branch-miss (gate-location 30 35 :branch))
           (statistics (gate-statistics 9 9 *defun-miss* branch-miss))
           (verdict (gate-verdict statistics))
           (allowance
             (list (list :file (getf verdict :file) :expression 0 :branch 0))))
      (expect (<= (getf *defun-miss* :start)
                  (getf branch-miss :start)
                  (getf branch-miss :end)
                  (getf *defun-miss* :end))
              :to-equal t)
      (expect (getf verdict :expression-shortfall) :to-equal 0)
      (expect (getf verdict :branch-shortfall) :to-equal 1)
      (expect (getf verdict :unexcused) :to-equal (list branch-miss))
      (expect (gate-status (list verdict) :rules (list (second *gate-rules*))
                           :allowance allowance)
              :to-equal 1)))

  (it "fails when an exclusion rule excused nothing"
    (let* ((verdict (gate-verdict (gate-statistics 9 10 *in-package-miss*)))
           (allowance
             (list (list :file (getf verdict :file) :expression 0 :branch 0))))
      (expect (gate-status (list verdict) :rules *gate-rules*
                           :allowance allowance)
              :to-equal 1)
      (expect (gate-report (list verdict) :rules *gate-rules*
                           :allowance allowance)
              :to-contain "stale exclusion: defun excused nothing")))

  (it "fails when no file was measured"
    (expect (gate-status nil :allowance nil) :to-equal 1))

  (it "rejects fake 50%/10% coverage through the production gate process"
    (multiple-value-bind (output exit-code)
        (run-coverage-reproduction-process "fake-gate.lisp")
      (expect exit-code :to-equal 1)
      (expect output :to-contain "fake coverage: expression 50%, branch 10%")
      (expect output :to-contain "shortfall changed for src/char-classes.lisp")
      (expect output :to-contain "coverage gate: FAIL")))

  (it "keeps nested runtime expressions uncovered when a synthetic whole-form marker is excluded"
    (multiple-value-bind (output exit-code)
        (run-coverage-reproduction-process "run.lisp")
      (expect exit-code :to-equal 0)
      (expect output :to-contain
              "synthetic whole-form exclusion leaves nested runtime expressions uncovered: PASS")
      (expect output :to-contain "coverage exclusion regression: PASS")))

  (it "loads the repository exclusions and per-file allowance"
    (flet ((data (name)
             (asdf:system-relative-pathname "cl-yaml-kit"
                                            (format nil "scripts/~A" name))))
      (expect (cl-yaml-kit/coverage-gate:read-exclusion-rules
               (data "coverage-exclusions.sexp"))
              :to-equal nil)
      (expect (cl-yaml-kit/coverage-gate:read-allowance
               (data "coverage-allowance.sexp"))
              :to-satisfy (function consp)))))
