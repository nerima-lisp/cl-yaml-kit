(unless (find-package "ASDF") (require :asdf))
(let* ((script-directory
          (uiop:pathname-directory-pathname *load-truename*))
       (root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-parent-directory-pathname script-directory))))
  (progn
    (uiop:chdir root)
    (load (merge-pathnames "scripts/coverage-gate.lisp" root))))

(let* ((script-directory (uiop:pathname-directory-pathname *load-truename*))
       (root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-parent-directory-pathname script-directory)))
       (source-directory (merge-pathnames "src/" root)))
  (let* ((rules (cl-yaml-kit/coverage-gate:read-exclusion-rules
                 (merge-pathnames "scripts/coverage-exclusions.sexp" root)))
         (allowance (cl-yaml-kit/coverage-gate:read-allowance
                     (merge-pathnames "scripts/coverage-allowance.sexp" root)))
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
             (mapcar
              (lambda (row)
                (let* ((file (getf row :file))
                       (entry (find file allowance
                                    :key (lambda (item) (getf item :file))
                                    :test #'string=))
                       (expression-allowance (if entry (getf entry :expression) 0))
                       (branch-allowance (if entry (getf entry :branch) 0))
                       (excluded-count (length (getf row :locations)))
                       (fake-low (string= file victim-file))
                       (expression-total (if fake-low
                                             (* 2 (1+ (+ expression-allowance excluded-count)))
                                             (+ 10 expression-allowance excluded-count)))
                       (expression-covered (if fake-low
                                               (/ expression-total 2)
                                               10))
                       (branch-total (if fake-low
                                         (* 10 (1+ branch-allowance))
                                         branch-allowance))
                       (branch-covered (if fake-low
                                           (floor branch-total 10)
                                           0)))
                  (list :pathname (getf row :pathname)
                        :expression-covered expression-covered
                        :expression-total expression-total
                        :branch-covered branch-covered
                        :branch-total branch-total
                        :uncovered-forms (getf row :locations))))
              rows))
           (verdicts (cl-yaml-kit/coverage-gate:gate-verdicts statistics rules)))
      (format t "fake coverage: expression 50%, branch 10% in ~A~%" victim-file)
      (cl-yaml-kit/coverage-gate:write-gate-report verdicts rules allowance *standard-output*)
      (uiop:quit (cl-yaml-kit/coverage-gate:gate-exit-status verdicts rules allowance)))))
