(unless (find-package "ASDF") (require :asdf))
(load (merge-pathnames "../coverage-gate.lisp" *load-truename*))

(load (merge-pathnames "project-root.lisp" *load-truename*))

(let* ((root (coverage-reproduction-project-root *load-truename*))
       (pathname (merge-pathnames "scripts/coverage-gate.lisp" root))
       (statistics
         (list (list :pathname pathname
                     :expression-covered 5
                     :expression-total 10
                     :branch-covered 1
                     :branch-total 10
                     :uncovered-forms nil)))
       (rules nil)
       (verdicts
         (cl-yaml-kit/coverage-gate:gate-verdicts
          statistics rules :expected-pathnames (list pathname))))
  (format t "fake coverage: expression 50%, branch 10% in ~A~%"
          (enough-namestring pathname root))
  (cl-yaml-kit/coverage-gate:write-gate-report verdicts rules *standard-output*)
  (uiop:quit (cl-yaml-kit/coverage-gate:gate-exit-status verdicts rules)))
