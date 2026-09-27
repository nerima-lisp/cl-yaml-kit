(require :asdf)

(defparameter *script-directory*
  (uiop:pathname-directory-pathname *load-truename*))
(defparameter *project-root*
  (uiop:pathname-parent-directory-pathname *script-directory*))

(asdf:load-asd (merge-pathnames "../cl-yaml-kit.asd" *script-directory*))
(asdf:load-asd
 (merge-pathnames "cl-weave/cl-weave.asd"
                  (pathname (uiop:getenv "CL_YAML_DEPS"))))
(asdf:load-system "cl-yaml-kit/test")

(defpackage #:yaml-kit/mutation
  (:use #:cl))
(in-package #:yaml-kit/mutation)

(defun source-forms (pathname)
  (let ((*package* (find-package :yaml-kit)))
    (with-open-file (stream pathname :direction :input)
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            collect form))))

(defun function-form (pathname name)
  (or (find-if (lambda (form)
                 (and (consp form)
                      (eq (first form) 'defun)
                      (eq (second form) name)))
               (source-forms pathname))
      (error "No DEFUN ~S in ~A" name pathname)))

(defun macro-form (pathname macro name)
  (let ((form (find-if (lambda (candidate)
                         (and (consp candidate)
                              (eq (first candidate) macro)
                              (eq (second candidate) name)))
                       (source-forms pathname))))
    (or (and form (macroexpand-1 form))
        (error "No ~S form for ~S in ~A" macro name pathname))))

(defun selected-test-count (name-filter)
  (length (cl-weave:collect-test-plan
           (cl-weave:root-suite)
           :name-filter name-filter)))

(defun run-selected-tests (name-filter)
  (cl-weave:run-all
   :reporter :spec
   :stream (make-broadcast-stream)
   :name-filter name-filter
   :max-workers 1
   :pass-with-no-tests nil))

(defun eval-definition (form)
  (handler-bind ((style-warning #'muffle-warning))
    (eval form)))

(defun mutation-results (name form name-filter)
  (let ((results
          (cl-weave:run-mutations
           form
           (lambda (mutated mutation)
             (declare (ignore mutation))
             (unwind-protect
                  (progn
                    (eval-definition mutated)
                    (run-selected-tests name-filter))
               (eval-definition form)))
           :timeout-ms 5000)))
    (format t "~A tests=~D ~S~%"
            name (selected-test-count name-filter)
            (cl-weave:mutation-summary results))
    (dolist (result results)
      (when (member (cl-weave:mutation-result-status result)
                    '(:survived :errored))
        (format t "  ~A ~S~%"
                (cl-weave:mutation-result-status result)
                (cl-weave:mutation-result-mutation result))))
    results))

(defun main ()
  (let* ((char-file (merge-pathnames "src/char-classes.lisp" cl-user::*project-root*))
         (schema-file (merge-pathnames "src/schema.lisp" cl-user::*project-root*))
         (targets
           (list
            (list "sc-alpha-p" (function-form char-file 'yaml-kit::sc-alpha-p)
                  "scanner")
            (list "sc-digit-p" (function-form char-file 'yaml-kit::sc-digit-p)
                  "scanner")
            (list "sc-hex-p" (function-form char-file 'yaml-kit::sc-hex-p)
                  "scanner")
            (list "%schema-resolve"
                  (function-form schema-file 'yaml-kit::%schema-resolve)
                  "loader schema")
            ))
         (all-results
           (mapcan (lambda (target)
                     (destructuring-bind (name form filter) target
                       (mutation-results name form filter)))
                   targets)))
    (format t "mutation-total targets=~D mutants=~D killed=~D survived=~D errored=~D~%"
            (length targets)
            (length all-results)
            (count :killed all-results :key #'cl-weave:mutation-result-status)
            (count :survived all-results :key #'cl-weave:mutation-result-status)
            (count :errored all-results :key #'cl-weave:mutation-result-status))))

(main)
