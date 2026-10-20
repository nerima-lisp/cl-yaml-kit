;;;; scripts/coverage-gate.lisp
;;;; Gate aggregate SB-COVER expression and branch coverage at 90%.
(defpackage #:cl-yaml-kit/coverage-gate
  (:use #:cl)
  (:export #:read-exclusion-rules #:top-level-forms #:file-verdict
           #:gate-verdicts #:unused-rules #:write-gate-report #:gate-exit-status))
(in-package #:cl-yaml-kit/coverage-gate)

(defparameter *whitespace-characters* '(#\Space #\Tab #\Newline #\Return #\Page))

(defun %read-data (pathname)
  (with-open-file (stream pathname :external-format :utf-8)
    (let ((*read-eval* nil))
      (loop with eof = (gensym)
            for datum = (read stream nil eof)
            until (eq datum eof)
            collect datum))))

(defun read-exclusion-rules (pathname)
  "Read the (:head NAME :reason TEXT) rules in PATHNAME."
  (let ((rules (%read-data pathname)))
    (dolist (rule rules rules)
      (unless (and (stringp (getf rule :head)) (stringp (getf rule :reason)))
        (error "Coverage exclusion ~S needs a string :head and :reason." rule)))))

(defun %source-key (pathname)
  (enough-namestring (pathname pathname) (uiop:getcwd)))

(defun %block-comment-start-p (char source position)
  (and (char= char #\#)
       (< (1+ position) (length source))
       (char= (char source (1+ position)) #\|)))

(defun %form-start (source position)
  (loop
    (when (>= position (length source))
      (return nil))
    (let ((char (char source position)))
      (cond ((member char *whitespace-characters*) (incf position))
            ((char= char #\;)
             (setf position (or (position #\Newline source :start position)
                                (length source))))
            ((%block-comment-start-p char source position)
             (error "Block comments are not supported by the coverage gate (offset ~D)."
                    position))
            (t (return position))))))

(defun %form-head (form)
  (and (consp form) (symbolp (car form))
       (string-downcase (symbol-name (car form)))))

(defun %read-top-level-form (source start)
  (multiple-value-bind (form end)
      (read-from-string source t nil :start start :preserve-whitespace t)
    (let ((head (%form-head form)))
      (when (equal head "in-package")
        (setf *package* (or (find-package (second form)) *package*)))
      (values (list :head head :start start :end end) end))))

(defun top-level-forms (source)
  "Return (:head :start :end) for each top-level form of SOURCE, tracking IN-PACKAGE."
  (let ((*package* (find-package '#:cl-user))
        (forms nil))
    (loop with position = (%form-start source 0)
          while position
          do (multiple-value-bind (form end)
                 (%read-top-level-form source position)
               (push form forms)
               (setf position (%form-start source end)))
          finally (return (nreverse forms)))))

(defun %spans-form-p (location form)
  (and (eq (getf location :kind) :expression)
       (= (getf location :start) (getf form :start))
       (= (getf location :end) (getf form :end))))

(defun %valid-file-statistics-p (statistics)
  (let ((pathname (getf statistics :pathname))
        (expression-covered (getf statistics :expression-covered))
        (expression-total (getf statistics :expression-total))
        (branch-covered (getf statistics :branch-covered))
        (branch-total (getf statistics :branch-total))
        (locations (getf statistics :uncovered-forms)))
    (and pathname
         (typep expression-covered '(integer 0))
         (typep expression-total '(integer 1))
         (<= expression-covered expression-total)
         (typep branch-covered '(integer 0))
         (typep branch-total '(integer 0))
         (<= branch-covered branch-total)
         (or (plusp branch-total) (zerop branch-covered))
         (listp locations))))

(defun %unexcused-forms (locations excluded)
  (remove-if (lambda (location)
               (some (lambda (form)
                       (%spans-form-p location form))
                     excluded))
             locations))

(defun %excluded-form-reports (source excluded)
  (mapcar (lambda (form)
            (list :head (getf form :head)
                  :line (1+ (count #\Newline source
                                   :end (getf form :start)))))
          excluded))

(defun %excluded-forms (forms heads locations)
  (remove-if-not
   (lambda (form)
     (and (member (getf form :head) heads :test #'equal)
          (find-if (lambda (location) (%spans-form-p location form))
                   locations)))
   forms))

(defun file-verdict (statistics source rules)
  "Classify one cl-weave COVERAGE-FILE-STATISTICS entry against RULES.
Only an uncovered expression spanning a complete excluded top-level form is
excused; nested expressions and every branch remain uncovered."
  (let* ((pathname (getf statistics :pathname))
         (expression-covered (getf statistics :expression-covered))
         (expression-total (getf statistics :expression-total))
         (branch-covered (getf statistics :branch-covered))
         (branch-total (getf statistics :branch-total))
         (locations (getf statistics :uncovered-forms))
         (heads (mapcar (lambda (rule) (getf rule :head)) rules)))
    (unless (%valid-file-statistics-p statistics)
      (error "Malformed or empty cl-weave coverage statistics for ~S." pathname))
    (let* ((forms (top-level-forms source))
           (excluded
             (%excluded-forms forms heads locations))
           (unexcused
             (%unexcused-forms locations excluded))
           (scored-expression-total (- expression-total (length excluded))))
      (when (> (length excluded) (- expression-total expression-covered))
        (error "Excluded expression count exceeds uncovered expressions for ~S."
               pathname))
      (list :file (enough-namestring (pathname pathname) (uiop:getcwd))
            :pathname pathname
            :expression-covered expression-covered
            :expression-total scored-expression-total
            :branch-covered branch-covered
            :branch-total branch-total
            :expression-shortfall (- scored-expression-total expression-covered)
            :branch-shortfall (- branch-total branch-covered)
            :excluded (%excluded-form-reports source excluded)
            :unexcused unexcused))))

(defun %duplicate-keys (keys)
  (remove-duplicates
   (remove-if (lambda (key) (= 1 (count key keys :test #'string=))) keys)
   :test #'string=))

(defun gate-verdicts (file-statistics rules
                      &key (expected-pathnames nil expected-pathnames-p))
  (unless (and (listp file-statistics) file-statistics)
    (error "Coverage measurement returned no source files."))
  (let* ((measured-keys
          (mapcar (lambda (statistics)
                    (%source-key (getf statistics :pathname)))
                  file-statistics))
         (duplicates (%duplicate-keys measured-keys)))
    (when duplicates
      (error "Duplicate measured source files: ~{~A~^, ~}." duplicates))
    (when expected-pathnames-p
      (let* ((expected-keys (mapcar #'%source-key expected-pathnames))
             (duplicate-expected (%duplicate-keys expected-keys))
             (missing (set-difference expected-keys measured-keys :test #'string=))
             (unexpected (set-difference measured-keys expected-keys :test #'string=)))
        (when duplicate-expected
          (error "Duplicate expected source files: ~{~A~^, ~}." duplicate-expected))
        (when (or missing unexpected)
          (error "Expected source coverage mismatch; missing: ~{~A~^, ~}; unexpected: ~{~A~^, ~}."
                 missing unexpected))))
    (mapcar (lambda (statistics)
              (file-verdict statistics
                            (uiop:read-file-string (getf statistics :pathname)
                                                   :external-format :utf-8)
                            rules))
            file-statistics)))

(defun unused-rules (verdicts rules)
  "RULES that excused nothing in VERDICTS; a stale rule fails the gate."
  (remove-if (lambda (rule)
               (some (lambda (verdict)
                       (find (getf rule :head) (getf verdict :excluded)
                             :key (lambda (form) (getf form :head)) :test #'equal))
                     verdicts))
             rules))

(defun %total (verdicts key)
  (reduce #'+ verdicts
          :key (lambda (verdict) (getf verdict key))
          :initial-value 0))
(defun %gate-passed-p (verdicts rules)
  (let ((expression-covered (%total verdicts :expression-covered))
        (expression-total (%total verdicts :expression-total))
        (branch-covered (%total verdicts :branch-covered))
        (branch-total (%total verdicts :branch-total))
        (files (mapcar (lambda (verdict) (getf verdict :file)) verdicts)))
    (and verdicts
         (null (%duplicate-keys files))
         (plusp expression-total)
         (plusp branch-total)
         (>= (* expression-covered 10) (* expression-total 9))
         (>= (* branch-covered 10) (* branch-total 9))
         (null (unused-rules verdicts rules)))))

(defun %write-locations (verdicts stream)
  (dolist (verdict verdicts)
    (let ((file (file-namestring (getf verdict :pathname))))
      (dolist (form (getf verdict :excluded))
        (format stream "excused ~A ~A:~D~%" (getf form :head) file (getf form :line)))
      (dolist (location (getf verdict :unexcused))
        (format stream "uncovered ~(~A~) ~A:~D:~D-~D:~D~%"
                (getf location :kind) file
                (getf location :line) (getf location :column)
                (getf location :end-line) (getf location :end-column))))))

(defun write-gate-report (verdicts rules stream)
  "Print per-file coverage, aggregate thresholds, stale rules, locations, and PASS or FAIL."
  (format stream "~&file~Texpression~Tbranch~%")
  (dolist (verdict verdicts)
    (format stream "~A~T~D/~D~T~D/~D~%"
            (getf verdict :file)
            (getf verdict :expression-covered) (getf verdict :expression-total)
            (getf verdict :branch-covered) (getf verdict :branch-total)))
  (%write-locations verdicts stream)
  (dolist (rule (unused-rules verdicts rules))
    (format stream "stale exclusion: ~A excused nothing~%" (getf rule :head)))
  (let ((expression-covered (%total verdicts :expression-covered))
        (expression-total (%total verdicts :expression-total))
        (branch-covered (%total verdicts :branch-covered))
        (branch-total (%total verdicts :branch-total)))
    (format stream "aggregate expression: ~D/~D (90% threshold) ~:[FAIL~;PASS~]~%"
            expression-covered expression-total
            (and (plusp expression-total)
                 (>= (* expression-covered 10) (* expression-total 9))))
    (format stream "aggregate branch: ~D/~D (90% threshold) ~:[FAIL~;PASS~]~%"
            branch-covered branch-total
            (and (plusp branch-total)
                 (>= (* branch-covered 10) (* branch-total 9)))))
  (format stream "coverage gate: ~:[FAIL~;PASS~]~%"
          (%gate-passed-p verdicts rules)))

(defun gate-exit-status (verdicts rules)
  "Return 0 when both aggregate coverage thresholds and all exclusions pass."
  (if (%gate-passed-p verdicts rules) 0 1))
