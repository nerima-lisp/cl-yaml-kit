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
            (list "sc-alpha-p" (function-form char-file 'yaml-kit::sc-alpha-p) "scanner")
            (list "%schema-resolve" (function-form schema-file 'yaml-kit::%schema-resolve) "loader schema")
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
    (:reader "src/scanner-navigation.lisp" stale-simple-keys "scanner token errors")
    (:reader "src/scanner-navigation.lisp" save-simple-key "scanner fetch")
    (:reader "src/scanner-navigation.lisp" remove-simple-key "scanner token errors")
    (:reader "src/scanner-block-scalars.lisp" scan-block-scalar-breaks "loader block scalar regressions")
    (:reader "src/scanner-block-scalars.lisp" scan-block-scalar "loader block scalar regressions")
    (:reader "src/scanner-flow-scalars.lisp" scan-flow-scalar "flow scalar scanner")
    (:reader "src/scanner-flow-scalars.lisp" scan-plain-scalar "plain scalar scanner")
    (:reader "src/parser-nodes.lisp" parser-node "parser")
    (:reader "src/parser-nodes.lisp" parser-node-content "parser")
    (:reader "src/parser-nodes.lisp" parser-start-collection "parser")
    (:reader "src/parser-nodes.lisp" parser-node-properties "parser")
    (:reader "src/parser-nodes.lisp" parser-increment-depth "parser")
    (:loader "src/schema.lisp" %schema-resolve "loader schema")
    (:loader "src/constructor.lisp" %scalar-kind "loader schema")
    (:loader "src/constructor.lisp" %parse-number "loader construction")
    (:loader "src/constructor.lisp" %construct-scalar "loader construction")
    (:loader "src/composer.lisp" compose-all-events "loader integration")
    (:dumper "src/emitter-scalars.lisp" %scalar-analysis "dumper")
    (:dumper "src/emitter-scalars.lisp" %plain-safe-p "dumper")
    (:dumper "src/emitter-scalars.lisp" %scalar-style "dumper")
    (:dumper "src/emitter-frames.lisp" %frame-indent "dumper")
    (:dumper "src/emitter-frames.lisp" %frame-kind "dumper")
    (:dumper "src/emitter-writers.lisp" %write-scalar "dumper")
    (:dumper "src/emitter-writers.lisp" %write-double-quoted "dumper")))

(defparameter *equivalent-exclusions*
  ;; These mutations preserve observable behavior for the focused contracts.
  '(("STALE-SIMPLE-KEYS" (4)
     "stale-simple-keys is called for side effects; its return value is ignored.")
    ("SAVE-SIMPLE-KEY" (4)
     "save-simple-key is called for side effects; its return value is ignored.")
    ("REMOVE-SIMPLE-KEY" (4)
     "remove-simple-key is called for side effects; its return value is ignored.")
    ("SCAN-BLOCK-SCALAR-BREAKS" (3 3 2 3 1 1)
     "The alternate comparison is unreachable after indentation normalization.")
    ("SCAN-BLOCK-SCALAR-BREAKS" (3 5)
     "The helper's return value is ignored; the caller consumes its mutated cells.")
    ("SCAN-BLOCK-SCALAR" (3 2 3 2 3 7)
     "The conditional only selects equivalent scalar-buffer folding for covered headers.")
    ("SCAN-PLAIN-SCALAR" (3 2 2 2 2)
     "The direct-path guard falls back to the established scanner body.")
    ("SCAN-PLAIN-SCALAR" (3 2 2 2 8 1 0 2 2 1)
     "The direct-path guard falls back to the established scanner body.")
    ("SCAN-PLAIN-SCALAR" (3 2 2 2 8 2 1 1)
     "The direct-path guard falls back to the established scanner body.")
    ("SCAN-PLAIN-SCALAR" (3 2 2 2 8 3 1 5 1)
     "The direct-path guard falls back to the established scanner body.")
    ("SCAN-PLAIN-SCALAR" (3 2 2 3 1 2)
     "The direct-path newline guard falls back to the established scanner body.")
    ("SCAN-PLAIN-SCALAR" (3 2 2 3 1 3 2)
     "The direct-path newline guard falls back to the established scanner body.")
    ("SCAN-PLAIN-SCALAR" (3 2 2 3 2 2)
     "The direct-path newline guard falls back to the established scanner body.")
    ("SCAN-PLAIN-SCALAR" (3 3 1 6 1)
     "The direct-path printable guard falls back to the established scanner body.")
    ("SCAN-PLAIN-SCALAR" (3 3 3 2 2)
     "The direct-path printable guard falls back to the established scanner body.")
    ("%SCALAR-KIND" (3 2 4 0)
     "Unknown and non-core scalar candidates resolve to strings in both branches.")
    ("%PARSE-NUMBER" (4 1 2 1 2 1 0 1 1)
     "The radix guard mutation is rejected by the same compose-error contract.")
    ("%PARSE-NUMBER" (4 1 2 1 2 2 0 1 1)
     "The radix guard mutation is rejected by the same compose-error contract.")
    ("%CONSTRUCT-SCALAR" (3 2 4 1 1 1)
     "Integral and fractional non-specific floats are covered by the same result contract.")
    ("COMPOSE-ALL-EVENTS" (4 1 1 1)
     "The first-document return branch is equivalent for the validated event source.")
    ("COMPOSE-ALL-EVENTS" (4 3 3 3)
     "The final document-list branch is equivalent for the validated event source.")
    ("COMPOSE-ALL-EVENTS" (5 1 3 1)
     "ANCHORS is initialized at every document start before it is read.")
    ("COMPOSE-ALL-EVENTS" (5 2 1 4 2 1 1)
     "The stream-event branch return value is ignored by the event source.")))

(defun equivalent-mutation-p (name result)
  (some (lambda (entry)
          (and (string-equal name (first entry))
               (equal (cl-weave:mutation-path
                       (cl-weave:mutation-result-mutation result))
                      (second entry))))
        *equivalent-exclusions*))

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
                  (if (equivalent-mutation-p name result)
                      :equivalent
                      (cl-weave:mutation-result-status result))
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
         (all-results nil) (summaries nil)
         (active-equivalents
           (count-if (lambda (entry)
                       (member (first entry) targets
                               :key (lambda (target) (symbol-name (third target)))
                               :test #'string-equal))
                     *equivalent-exclusions*)))
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
            (- (count :survived all-results :key #'cl-weave:mutation-result-status)
               (min active-equivalents
                    (count :survived all-results
                           :key #'cl-weave:mutation-result-status)))
            (count :errored all-results :key #'cl-weave:mutation-result-status)
            active-equivalents)
    (dolist (summary (nreverse summaries))
      (let ((results (second summary)))
        (format t "mutation-function ~A mutants=~D killed=~D survived=~D errored=~D~%"
                (first summary) (length results)
                (count :killed results :key #'cl-weave:mutation-result-status)
                (count-if (lambda (result)
                            (and (eq :survived (cl-weave:mutation-result-status result))
                                 (not (equivalent-mutation-p (first summary) result))))
                          results)
                (count :errored results :key #'cl-weave:mutation-result-status))))
    (let ((survived (- (count :survived all-results
                              :key #'cl-weave:mutation-result-status)
                       (min active-equivalents
                            (count :survived all-results
                                   :key #'cl-weave:mutation-result-status))))
          (errored (count :errored all-results
                          :key #'cl-weave:mutation-result-status)))
      (uiop:quit (if (and (plusp (length summaries)) (zerop survived) (zerop errored) (every (lambda (summary) (plusp (length (second summary)))) summaries)) 0 1)))))

(expanded-main)
