;;;; run-tests.lisp
(require :asdf)
(defun script-directory ()
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *compile-file-truename*
                               (error "Unable to determine script location"))))
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(script-directory)) :inherit-configuration))

(defun compile-warning-files (root)
  (list (merge-pathnames "run-tests.lisp" root)
        (merge-pathnames "scripts/run-coverage.lisp" root)
        (merge-pathnames "scripts/run-mutation.lisp" root)))

(defun compile-warning-output-file (file)
  (merge-pathnames
   (make-pathname :name (format nil "~A-check" (pathname-name file))
                  :type "fasl")
   (uiop:temporary-directory)))

(defun compile-warning-entry (condition)
  (when *compile-file-truename*
    (list *compile-file-truename* condition)))

(defun collect-compile-warnings (thunk)
  (let ((warnings nil))
    (handler-bind
        ((warning
           (lambda (condition)
             (let ((entry (compile-warning-entry condition)))
               (when entry
                 (push entry warnings)
                 (muffle-warning condition))))))
      (funcall thunk))
    warnings))

(defun compile-project-for-warning-check (root)
  (collect-compile-warnings
   (lambda ()
     (asdf:operate 'asdf:compile-op "cl-yaml-kit" :force t)
     (asdf:operate 'asdf:compile-op "cl-yaml-kit/test" :force t)
     (dolist (file (compile-warning-files root))
       (compile-file file :output-file (compile-warning-output-file file))))))

(defun partition-compile-warnings (warnings source-directory)
  (let ((non-style
          (remove-if (lambda (entry)
                      (typep (second entry) 'style-warning))
                     warnings))
        (source-style
          (remove-if-not
           (lambda (entry)
             (and (typep (second entry) 'style-warning)
                  (uiop:string-prefix-p
                   (namestring source-directory)
                   (namestring (first entry)))))
           warnings)))
    (values non-style source-style)))

(defun report-compile-warnings (non-style source-style)
  (format t "compile warning check: non-style=~D src-style=~D~%"
          (length non-style) (length source-style))
  (dolist (entry (append non-style source-style))
    (format *error-output* "~&compile warning in ~A: ~A~%"
            (namestring (first entry)) (second entry)))
  (when (or non-style source-style)
    (error "Compilation warning check failed: non-style=~D src-style=~D"
           (length non-style) (length source-style))))

(defun compile-warning-check ()
  (let* ((root (script-directory))
         (source-directory (merge-pathnames "src/" root))
         (warnings (compile-project-for-warning-check root)))
    (multiple-value-bind (non-style source-style)
        (partition-compile-warnings warnings source-directory)
      (report-compile-warnings non-style source-style))))

(compile-warning-check)
(asdf:test-system "cl-yaml-kit")
(uiop:quit 0)
