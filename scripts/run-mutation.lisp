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
        (format t "  ~A operator=~A path=~S~%"
                (cl-weave:mutation-result-status result)
                (cl-weave:mutation-operator
                 (cl-weave:mutation-result-mutation result))
                (cl-weave:mutation-path
                 (cl-weave:mutation-result-mutation result)))))
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

(defparameter *expanded-targets*
  '((:reader "src/char-classes.lisp" sc-alpha-p "scanner character classes")
    (:reader "src/char-classes.lisp" sc-digit-p "scanner character classes")
    (:reader "src/char-classes.lisp" sc-hex-p "scanner character classes")
    (:reader "src/scanner.lisp" stale-simple-keys "scanner")
    (:reader "src/scanner.lisp" save-simple-key "scanner")
    (:reader "src/scanner.lisp" remove-simple-key "scanner")
    (:reader "src/scanner-block-scalars.lisp" scan-block-scalar-breaks "scanner")
    (:reader "src/scanner-block-scalars.lisp" scan-block-scalar "scanner")
    (:reader "src/scanner-flow-scalars.lisp" scan-flow-scalar "scanner")
    (:reader "src/scanner-flow-scalars.lisp" scan-plain-scalar "scanner")
    (:reader "src/parser.lisp" parser-node "parser")
    (:reader "src/parser.lisp" parser-node-content "parser")
    (:reader "src/parser.lisp" parser-start-collection "parser")
    (:reader "src/parser.lisp" parser-node-properties "parser")
    (:reader "src/parser.lisp" parser-increment-depth "parser")
    (:loader "src/schema.lisp" %schema-resolve "loader schema")
    (:loader "src/constructor.lisp" %scalar-kind "loader construction")
    (:loader "src/constructor.lisp" %parse-number "loader construction")
    (:loader "src/constructor.lisp" %construct-scalar "loader construction")
    (:loader "src/constructor.lisp" %construct-mapping "loader construction")
    (:loader "src/composer.lisp" compose-events "loader integration")
    (:loader "src/composer.lisp" compose-all-events "loader integration")
    (:dumper "src/emitter-scalars.lisp" %scalar-analysis "dumper")
    (:dumper "src/emitter-scalars.lisp" %plain-safe-p "dumper")
    (:dumper "src/emitter-scalars.lisp" %scalar-style "dumper")
    (:dumper "src/emitter-frames.lisp" %frame-indent "dumper")
    (:dumper "src/emitter-frames.lisp" %frame-kind "dumper")
    (:dumper "src/emitter-writers.lisp" %write-scalar "dumper")
    (:dumper "src/emitter-writers.lisp" %write-double-quoted "dumper")))

(defparameter *equivalent-exclusions*
  ;; Each entry is (function mutation reason); keep this empty unless evidence
  ;; proves that a surviving mutation is behavior-preserving.
  nil)

(defun expanded-mutation-results (name form name-filter)
  (let ((test-count (selected-test-count name-filter)))
    (unless (plusp test-count)
      (error "No tests selected for mutation target ~A (filter ~S)."
             name name-filter))
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
      #+sbcl (sb-ext:gc :full t)
      (format t "~A tests=~D ~S~%" name test-count
              (cl-weave:mutation-summary results))
      (dolist (result results)
        (when (member (cl-weave:mutation-result-status result)
                      '(:survived :errored))
          (format t "  ~A operator=~A path=~S~%"
                  (cl-weave:mutation-result-status result)
                  (cl-weave:mutation-operator
                   (cl-weave:mutation-result-mutation result))
                  (cl-weave:mutation-path
                   (cl-weave:mutation-result-mutation result)))))
      results)))

(defun expanded-main ()
  (let* ((area-filter (uiop:getenv "CL_YAML_MUTATION_AREA"))
         (targets (remove-if-not
                   (lambda (target)
                     (or (null area-filter)
                         (string-equal area-filter
                                       (symbol-name (first target)))))
                   *expanded-targets*))
         (all-results nil) (summaries nil))
    (dolist (target targets)
      (destructuring-bind (area relative-file name name-filter) target
        (declare (ignore area))
        (let ((results
                (expanded-mutation-results
                 (symbol-name name)
                (function-form (merge-pathnames relative-file cl-user::*project-root*)
                                (intern (symbol-name name) :yaml-kit))
                 name-filter)))
          #+sbcl (sb-ext:gc :full t)
          (push (list (symbol-name name) results) summaries)
          (setf all-results (append results all-results)))))
    (format t "mutation-total targets=~D mutants=~D killed=~D survived=~D errored=~D equivalent=~D~%"
            (length targets) (length all-results)
            (count :killed all-results :key #'cl-weave:mutation-result-status)
            (count :survived all-results :key #'cl-weave:mutation-result-status)
            (count :errored all-results :key #'cl-weave:mutation-result-status)
            (length *equivalent-exclusions*))
    (dolist (summary (nreverse summaries))
      (let ((results (second summary)))
        (format t "mutation-function ~A mutants=~D killed=~D survived=~D errored=~D~%"
                (first summary) (length results)
                (count :killed results :key #'cl-weave:mutation-result-status)
                (count :survived results :key #'cl-weave:mutation-result-status)
                (count :errored results :key #'cl-weave:mutation-result-status))))
    (let ((survived (count :survived all-results
                           :key #'cl-weave:mutation-result-status))
          (errored (count :errored all-results
                          :key #'cl-weave:mutation-result-status)))
      (uiop:quit (if (and (zerop survived) (zerop errored)) 0 1)))))

(expanded-main)
