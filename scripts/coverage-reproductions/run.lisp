(unless (find-package "ASDF") (require :asdf))

(unless (find-package "SB-COVER") (require :sb-cover))
(load (merge-pathnames "../coverage-gate.lisp" *load-truename*))

(let* ((script-directory (uiop:pathname-directory-pathname *load-truename*))
       (root (uiop:pathname-parent-directory-pathname
              (uiop:pathname-parent-directory-pathname script-directory)))
       (fixture-directory (merge-pathnames "scripts/coverage-reproductions/" root))
       (fixture-name "nested-runtime-expressions.lisp"))
  (unless (find-package "CL-WEAVE") (asdf:load-system "cl-weave"))
  (let* ((fixture (merge-pathnames fixture-name fixture-directory))
         (fasl (merge-pathnames
                (format nil "coverage-repro-nested-~D.fasl" (random 1000000000))
                (uiop:temporary-directory)))
         (source (uiop:read-file-string fixture :external-format :utf-8))
         (form (first (cl-yaml-kit/coverage-gate:top-level-forms source)))
         (rule (list :head "defparameter"
                     :reason "synthetic whole-form regression case")))
    (unless (and form (equal (getf form :head) "defparameter"))
      (error "The nested runtime fixture must start with a DEFPARAMETER form."))
    (defvar *coverage-repro-false* nil)
    (proclaim (read-from-string
               "(optimize (sb-cover:store-coverage-data 3))"))
    (unwind-protect
         (progn
           (uiop:symbol-call :cl-weave :reset-coverage)
           (multiple-value-bind (output warnings failure)
               (compile-file fixture :output-file fasl)
             (declare (ignore warnings))
             (when failure
               (error "Could not compile the nested runtime coverage reproduction."))
             (load output))
           (let* ((statistics
                    (uiop:symbol-call :cl-weave :coverage-file-statistics
                                      :include-pathnames (list fixture-directory)))
                  (file-statistics
                    (find (file-namestring fixture) statistics
                          :key (lambda (item)
                                 (file-namestring (getf item :pathname)))
                          :test (function string=))))
             (unless file-statistics
               (error "SB-COVER recorded no statistics for the nested runtime fixture."))
             (let* ((nested-expression
                      (find-if
                       (lambda (location)
                         (and (eq (getf location :kind) :expression)
                              (> (getf location :start) (getf form :start))
                              (< (getf location :end) (getf form :end))))
                       (getf file-statistics :uncovered-forms)))
                    (whole-form-location
                      (list :kind :expression
                            :start (getf form :start)
                            :end (getf form :end)))
                    (test-statistics (copy-list file-statistics)))
               (unless nested-expression
                 (error "SB-COVER reported no nested runtime expression in the fixture."))
               (setf (getf test-statistics :uncovered-forms)
                     (cons whole-form-location
                           (getf file-statistics :uncovered-forms)))
               (let ((verdict
                       (cl-yaml-kit/coverage-gate:file-verdict
                        test-statistics source (list rule))))
                 (unless (find "defparameter" (getf verdict :excluded)
                               :key (lambda (item) (getf item :head))
                               :test (function equal))
                   (error "The gate did not recognize the synthetic whole-form exclusion."))
                 (unless (member nested-expression (getf verdict :unexcused)
                                 :test (function equal))
                   (error "The gate hid a nested runtime expression in the excluded form."))
                 (format t "synthetic whole-form exclusion leaves nested runtime expressions uncovered: PASS~%")))))
      (when (probe-file fasl)
        (delete-file fasl))))
  (format t "coverage exclusion regression: PASS~%"))
