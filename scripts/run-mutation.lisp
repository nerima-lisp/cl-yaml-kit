;;;; scripts/run-mutation.lisp
;;;; Mutate selected src/ functions; every mutant must be killed by the tests its
;;;; name filter selects, or match a recorded equivalent mutation.
(require :asdf)

(asdf:load-asd (merge-pathnames "../cl-yaml-kit.asd" (uiop:pathname-directory-pathname
                                                      *load-truename*)))
(asdf:load-system "cl-yaml-kit/test")

(defpackage #:cl-yaml-kit/mutation
  (:use #:cl))
(in-package #:cl-yaml-kit/mutation)

(defparameter *project-root*
  (uiop:pathname-parent-directory-pathname (uiop:pathname-directory-pathname *load-truename*)))

(defparameter *mutant-timeout-ms* 5000)
(define-condition mutation-process-timeout (error) ()
  (:report "Mutant subprocess timed out"))

;;; (AREA FILE FUNCTION TEST-NAME-FILTER); CL_YAML_MUTATION_AREA selects one AREA.
(defparameter *targets*
  '((:reader "src/char-classes.lisp" sc-alpha-p "scanner character classes")
    (:reader "src/scanner-navigation.lisp" stale-simple-keys "scanner token errors")
    (:reader "src/scanner-navigation.lisp" save-simple-key "scanner fetch")
    (:reader "src/scanner-navigation.lisp" remove-simple-key "scanner token errors")
    (:reader "src/scanner-block-scalars.lisp" scan-block-scalar-breaks
     "loader block scalar regressions")
    (:reader "src/scanner-block-scalars.lisp" scan-block-scalar "block scalar")
    (:reader "src/scanner-flow-scalars.lisp" scan-flow-scalar "flow scalar scanner")
    (:reader "src/scanner-flow-scalars.lisp" scan-plain-scalar "scanner")
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

;;; (FUNCTION MUTATION-PATH REASON): surviving mutants that preserve observable behavior.
(defparameter *equivalent-mutations*
  '((scan-block-scalar-breaks (3 3 2 3 1 1)
     "The alternate comparison is unreachable after indentation normalization.")
    (stale-simple-keys (4)
     "Called for side effects; its return value is ignored.")
    (save-simple-key (4)
     "Called for side effects; its return value is ignored.")
    (remove-simple-key (4)
     "Called for side effects; its return value is ignored.")
    (scan-block-scalar-breaks (3 5)
     "The helper's return value is ignored; the caller consumes its mutated cells.")
    (scan-plain-scalar (3 2 2 2 2)
     "The direct-path guard falls back to the scanner body.")
    (scan-plain-scalar (3 2 2 2 8 1 0 2 2 1)
     "The direct-path guard falls back to the scanner body.")
    (scan-plain-scalar (3 2 2 2 8 2 1 1)
     "The direct-path guard falls back to the scanner body.")
    (scan-plain-scalar (3 2 2 2 8 3 1 5 1)
     "The direct-path guard falls back to the scanner body.")
    (scan-plain-scalar (3 2 2 3 1 2)
     "The direct-path newline guard falls back to the scanner body.")
    (scan-plain-scalar (3 2 2 3 1 3 2)
     "The direct-path newline guard falls back to the scanner body.")
    (scan-plain-scalar (3 3 1 6 1)
     "The direct-path printable guard falls back to the scanner body.")
    (%scalar-kind (3 2 4 0)
     "Unknown and non-core scalar candidates resolve to strings in both branches.")
    (%parse-number (4 1 2 1 2 1 0 1 1)
     "The radix guard mutation is rejected by the same compose-error contract.")
    (%parse-number (4 1 2 1 2 2 0 1 1)
     "The radix guard mutation is rejected by the same compose-error contract.")
    (%construct-scalar (3 2 4 1 1 1)
     "Integral and fractional non-specific floats share the same result contract.")
    (compose-all-events (4 1 1 1)
     "The first-document return branch is equivalent for the validated event source.")
    (compose-all-events (4 3 3 3)
     "The final document-list branch is equivalent for the validated event source.")))

(defun function-form (pathname name)
  (let ((*package* (find-package '#:yaml-kit)))
    (with-open-file (stream pathname :external-format :utf-8)
      (or (loop for form = (read stream nil stream)
                until (eq form stream)
                when (and (consp form) (eq (first form) 'defun)
                          (string= (symbol-name (second form)) (symbol-name name)))
                  return form)
          (error "No DEFUN ~A in ~A" name pathname)))))

(defun mutation-child-form (mutated name-filter)
  `(progn
     (fmakunbound ',(second mutated))
     (eval ',mutated)
     (let ((passed
             (cl-weave:run-all :reporter :spec :stream (make-broadcast-stream)
                               :name-filter ,name-filter :max-workers 1
                               :pass-with-no-tests nil)))
       (write-line (if passed
                       "CL-YAML-MUTATION-RESULT:SURVIVED"
                       "CL-YAML-MUTATION-RESULT:KILLED"))
       (finish-output))))
(defun run-isolated-mutant (mutated name-filter)
  (let* ((result
           (cl-weave:run-isolated
            (mutation-child-form mutated name-filter)
            :systems '("cl-yaml-kit/test")
            :package "YAML-KIT"
            :timeout (/ *mutant-timeout-ms* 1000.0)))
         (output (cl-weave:isolated-result-stdout result))
         (error-output (cl-weave:isolated-result-stderr result)))
    (case (cl-weave:isolated-result-status result)
      (:timeout (error 'mutation-process-timeout))
      (:fail
       (error "Mutant child failed with exit code ~D.~%stdout:~%~A~%stderr:~%~A"
              (cl-weave:isolated-result-exit-code result) output error-output))
      (:pass
       (cond ((search "CL-YAML-MUTATION-RESULT:SURVIVED" output) t)
             ((search "CL-YAML-MUTATION-RESULT:KILLED" output) nil)
             (t (error "Mutant child exited without a result marker.~%stdout:~%~A~%stderr:~%~A"
                       output error-output))))
      (otherwise
       (error "Unexpected isolated mutant status: ~S"
              (cl-weave:isolated-result-status result))))))

(defun mutation-path (result)
  (cl-weave:mutation-path (cl-weave:mutation-result-mutation result)))

(defun equivalent-entry (name result)
  (find-if (lambda (entry)
             (and (string= (symbol-name (first entry)) (symbol-name name))
                  (equal (second entry) (mutation-path result))))
           *equivalent-mutations*))

(defun status (name result)
  (let ((mutation-status (cl-weave:mutation-result-status result))
        (condition (cl-weave:mutation-result-condition result)))
    (cond ((typep condition 'mutation-process-timeout) :timeout)
          ((and (eq mutation-status :survived) (equivalent-entry name result)) :equivalent)
          (t mutation-status))))
(defun target-summary (name results)
  (let ((statuses (mapcar (lambda (result) (status name result)) results)))
    (list :total (length results)
          :killed (count :killed statuses)
          :survived (count :survived statuses)
          :errored (count :errored statuses)
          :timeouts (count :timeout statuses)
          :equivalent (count :equivalent statuses))))

(defun mutate-target (target)
  "Run every mutant of TARGET's function and return (NAME . RESULTS)."
  (destructuring-bind (area file name name-filter) target
    (declare (ignore area))
    (let ((form (function-form (merge-pathnames file *project-root*) name))
          (test-count (length (cl-weave:collect-test-plan (cl-weave:root-suite)
                                                          :name-filter name-filter))))
      (unless (plusp test-count)
        (error "No tests selected for mutation target ~A (filter ~S)." name name-filter))
      (let ((results (cl-weave:run-mutations
                      form
                      (lambda (mutated mutation)
                        (declare (ignore mutation))
                        (run-isolated-mutant mutated name-filter)))))
        (sb-ext:gc :full t)
        (format t "~(~A~) tests=~D ~S~%"
                name test-count (target-summary name results))
        (dolist (result results)
          (unless (eq (status name result) :killed)
            (format t "  ~(~A~) operator=~A path=~S~%" (status name result)
                    (cl-weave:mutation-operator (cl-weave:mutation-result-mutation result))
                    (mutation-path result))))
        (cons name results)))))

(defun stale-equivalents (target-results)
  (remove-if-not
   (lambda (entry)
     (let ((target (assoc (first entry) target-results)))
       (and target
            (let ((matching-results
                    (remove-if-not
                     (lambda (result)
                       (equal (second entry) (mutation-path result)))
                     (cdr target))))
              (or (null matching-results)
                  (some (lambda (result)
                          (eq (status (first entry) result) :killed))
                        matching-results))))))
   *equivalent-mutations*))

(defun main ()
  (let* ((area (uiop:getenv "CL_YAML_MUTATION_AREA"))
         (targets (remove-if-not (lambda (target)
                                   (or (null area) (string-equal area (first target))))
                                 *targets*))
         (started-at (get-internal-real-time))
         (target-results (mapcar #'mutate-target targets))
         (statuses (loop for (name . results) in target-results
                         append (mapcar (lambda (result) (status name result)) results)))
         (stale (stale-equivalents target-results)))
    (dolist (entry stale)
      (format t "stale equivalent mutation: ~(~A~) path=~S~%"
              (first entry) (second entry)))
    (format t "mutation-total targets=~D mutants=~D killed=~D survived=~D errored=~D timeouts=~D equivalent=~D mutant-timeout-ms=~D elapsed-seconds=~D~%"
            (length targets) (length statuses) (count :killed statuses)
            (count :survived statuses) (count :errored statuses)
            (count :timeout statuses) (count :equivalent statuses)
            *mutant-timeout-ms*
            (round (/ (- (get-internal-real-time) started-at)
                      internal-time-units-per-second)))
    (uiop:quit (if (and targets
                        (every #'cdr target-results)
                        (zerop (count :survived statuses))
                        (zerop (count :errored statuses))
                        (zerop (count :timeout statuses))
                        (null stale))
                   0
                   1))))

(main)
