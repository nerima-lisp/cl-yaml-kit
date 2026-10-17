;;;; scripts/coverage-gate.lisp
;;;; Judge cl-weave per-file coverage statistics against the 100% policy, tolerating
;;;; at most the recorded allowance while that policy is not yet met.
(defpackage #:cl-yaml-kit/coverage-gate
  (:use #:cl)
  (:export #:read-exclusion-rules #:read-allowance #:top-level-forms #:file-verdict
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

(defun read-allowance (pathname)
  "Read the (:expression N :branch M) shortfall the gate tolerates while 100% is pending."
  (let ((allowance (first (%read-data pathname))))
    (unless (and (typep (getf allowance :expression) '(integer 0))
                 (typep (getf allowance :branch) '(integer 0)))
      (error "Coverage allowance ~S needs non-negative :expression and :branch counts."
             allowance))
    allowance))

(defun %form-start (source position)
  (loop
    (when (>= position (length source))
      (return nil))
    (let ((char (char source position)))
      (cond ((member char *whitespace-characters*) (incf position))
            ((char= char #\;)
             (setf position (or (position #\Newline source :start position)
                                (length source))))
            ((and (char= char #\#)
                  (< (1+ position) (length source))
                  (char= (char source (1+ position)) #\|))
             (error "Block comments are not supported by the coverage gate (offset ~D)."
                    position))
            (t (return position))))))

(defun %form-head (form)
  (and (consp form) (symbolp (car form))
       (string-downcase (symbol-name (car form)))))

(defun top-level-forms (source)
  "Return (:head :start :end) for each top-level form of SOURCE, tracking IN-PACKAGE."
  (let ((*package* (find-package '#:cl-user))
        (forms nil))
    (do ((start (%form-start source 0)))
        ((null start) (nreverse forms))
      (multiple-value-bind (form end)
          (read-from-string source t nil :start start :preserve-whitespace t)
        (let ((head (%form-head form)))
          (when (equal head "in-package")
            (setf *package* (or (find-package (second form)) *package*)))
          (push (list :head head :start start :end end) forms))
        (setf start (%form-start source end))))))

(defun %spans-form-p (location form)
  (and (eq (getf location :kind) :expression)
       (= (getf location :start) (getf form :start))
       (= (getf location :end) (getf form :end))))

(defun %within-form-p (location form)
  (and (eq (getf location :kind) :expression)
       (<= (getf form :start) (getf location :start))
       (<= (getf location :end) (getf form :end))))

(defun file-verdict (statistics source rules)
  "Classify one cl-weave COVERAGE-FILE-STATISTICS entry against RULES.
An uncovered expression is excused only inside a top-level form whose head a
rule names and which is itself reported unexecuted as a whole; branches are
never excused."
  (let* ((heads (mapcar (lambda (rule) (getf rule :head)) rules))
         (locations (getf statistics :uncovered-forms))
         (excluded (remove-if-not
                    (lambda (form)
                      (and (member (getf form :head) heads :test #'equal)
                           (find-if (lambda (location) (%spans-form-p location form))
                                    locations)))
                    (top-level-forms source)))
         (unexcused (remove-if (lambda (location)
                                 (some (lambda (form) (%within-form-p location form))
                                       excluded))
                               locations)))
    (list :pathname (getf statistics :pathname)
          :expression-covered (getf statistics :expression-covered)
          :expression-total (getf statistics :expression-total)
          :branch-covered (getf statistics :branch-covered)
          :branch-total (getf statistics :branch-total)
          :expression-shortfall (- (getf statistics :expression-total)
                                   (getf statistics :expression-covered)
                                   (- (length locations) (length unexcused)))
          :branch-shortfall (- (getf statistics :branch-total)
                               (getf statistics :branch-covered))
          :excluded (mapcar (lambda (form)
                              (list :head (getf form :head)
                                    :line (1+ (count #\Newline source
                                                     :end (getf form :start)))))
                            excluded)
          :unexcused unexcused)))

(defun gate-verdicts (file-statistics rules)
  (mapcar (lambda (statistics)
            (file-verdict statistics
                          (uiop:read-file-string (getf statistics :pathname)
                                                 :external-format :utf-8)
                          rules))
          file-statistics))

(defun unused-rules (verdicts rules)
  "RULES that excused nothing in VERDICTS; a stale rule fails the gate."
  (remove-if (lambda (rule)
               (some (lambda (verdict)
                       (find (getf rule :head) (getf verdict :excluded)
                             :key (lambda (form) (getf form :head)) :test #'equal))
                     verdicts))
             rules))

(defun %total-shortfall (verdicts kind)
  (reduce #'+ verdicts :key (lambda (verdict) (getf verdict kind))))

(defun %gate-passed-p (verdicts rules allowance)
  (and verdicts
       (null (unused-rules verdicts rules))
       (<= (%total-shortfall verdicts :expression-shortfall) (getf allowance :expression))
       (<= (%total-shortfall verdicts :branch-shortfall) (getf allowance :branch))))

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

(defun write-gate-report (verdicts rules allowance stream)
  "Print one row per file, every excused form and unexcused uncovered form, the
totals against ALLOWANCE, stale RULES, and PASS or FAIL."
  (format stream "~&file~Texpression~Tbranch~Texpression-shortfall~Tbranch-shortfall~%")
  (dolist (verdict verdicts)
    (format stream "~A~T~D/~D~T~D/~D~T~D~T~D~%"
            (file-namestring (getf verdict :pathname))
            (getf verdict :expression-covered) (getf verdict :expression-total)
            (getf verdict :branch-covered) (getf verdict :branch-total)
            (getf verdict :expression-shortfall) (getf verdict :branch-shortfall)))
  (%write-locations verdicts stream)
  (dolist (rule (unused-rules verdicts rules))
    (format stream "stale exclusion: ~A excused nothing~%" (getf rule :head)))
  (let ((expression (%total-shortfall verdicts :expression-shortfall))
        (branch (%total-shortfall verdicts :branch-shortfall)))
    (format stream "coverage shortfall: expression ~D (allowance ~D), branch ~D (allowance ~D)~%"
            expression (getf allowance :expression) branch (getf allowance :branch))
    (format stream "100% coverage: ~:[not met~;met~]~%"
            (and verdicts (zerop (+ expression branch))))
    (when (and (%gate-passed-p verdicts rules allowance)
               (or (< expression (getf allowance :expression))
                   (< branch (getf allowance :branch))))
      (format stream "allowance can be lowered to (:expression ~D :branch ~D)~%"
              expression branch)))
  (format stream "coverage gate: ~:[FAIL~;PASS~]~%" (%gate-passed-p verdicts rules allowance)))

(defun gate-exit-status (verdicts rules allowance)
  "0 when VERDICTS is non-empty, every rule excused something, and the total
shortfall is within ALLOWANCE; 1 otherwise."
  (if (%gate-passed-p verdicts rules allowance) 0 1))
