(unless (find-package "ASDF") (require :asdf))
(load (merge-pathnames "../coverage-gate.lisp" *load-truename*))

(let* ((script-directory (uiop:pathname-directory-pathname *load-truename*))
       (root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-parent-directory-pathname script-directory)))
       (source-directory (merge-pathnames "src/" root))
       (rules (cl-yaml-kit/coverage-gate:read-exclusion-rules
               (merge-pathnames "scripts/coverage-exclusions.sexp" root)))
       (sources (sort (directory (merge-pathnames "*.lisp" source-directory))
                      #'string< :key #'namestring))
       (rows
         (mapcar (lambda (pathname)
                   (let ((source (uiop:read-file-string pathname :external-format :utf-8)))
                     (list :pathname pathname
                           :file (enough-namestring pathname root)
                           :source source
                           :forms (cl-yaml-kit/coverage-gate:top-level-forms source)
                           :locations nil)))
                 sources)))
  (dolist (rule rules)
    (let* ((head (getf rule :head))
           (row (find-if (lambda (candidate)
                           (find head (getf candidate :forms)
                                 :key (lambda (form) (getf form :head))
                                 :test #'equal))
                         rows)))
      (unless row
        (error "Cannot exercise configured exclusion ~A in src/." head))
      (let* ((form (find head (getf row :forms)
                         :key (lambda (item) (getf item :head))
                         :test #'equal))
             (source (getf row :source))
             (start (getf form :start))
             (end (getf form :end))
             (line (1+ (count #\Newline source :end start)))
             (line-start (or (position #\Newline source :end start :from-end t) -1))
             (end-line (1+ (count #\Newline source :end end)))
             (end-line-start (or (position #\Newline source :end end :from-end t) -1))
             (location (list :kind :expression :start start :end end
                             :line line :column (- start line-start)
                             :end-line end-line :end-column (- end end-line-start))))
        (push location (getf row :locations)))))
  (let* ((victim (first rows))
         (victim-file (getf victim :file))
         (statistics
           (mapcar (lambda (row)
                     (list :pathname (getf row :pathname)
                           :expression-covered 5
                           :expression-total 10
                           :branch-covered 1
                           :branch-total 10
                           :uncovered-forms (getf row :locations)))
                   rows))
         (verdicts (cl-yaml-kit/coverage-gate:gate-verdicts
                    statistics rules :expected-pathnames sources)))
    (format t "fake coverage: expression 50%, branch 10% in ~A~%" victim-file)
    (cl-yaml-kit/coverage-gate:write-gate-report verdicts rules *standard-output*)
    (uiop:quit (cl-yaml-kit/coverage-gate:gate-exit-status verdicts rules))))
