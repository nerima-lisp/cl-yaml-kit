;;;; scripts/coverage-gate.lisp
;;;; Compare per-file SB-COVER shortfalls with their exact recorded allowances.
;;;; Each file's expression and branch counts must match its ratchet.
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
  "Read exact expression and branch shortfalls allowed for each source file."
  (let* ((documents (%read-data pathname))
         (allowance (first documents))
         (files (and (listp allowance) (getf allowance :files))))
    (unless (and (= (length documents) 1)
                 (listp allowance)
                 (listp files)
                 (every (lambda (entry)
                          (and (listp entry)
                               (stringp (getf entry :file))
                               (plusp (length (getf entry :file)))
                               (typep (getf entry :expression) '(integer 0))
                               (typep (getf entry :branch) '(integer 0))))
                        files))
      (error "Coverage allowance ~S needs unique file counts."
             allowance))
    (let ((keys (mapcar (lambda (entry) (getf entry :file)) files)))
      (unless (= (length keys) (length (remove-duplicates keys :test #'string=)))
        (error "Coverage allowance ~S contains duplicate file entries." pathname)))
    files))

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
    (unless (and pathname
                 (typep expression-covered '(integer 0))
                 (typep expression-total '(integer 1))
                 (<= expression-covered expression-total)
                 (typep branch-covered '(integer 0))
                 (typep branch-total '(integer 0))
                 (<= branch-covered branch-total)
                 (or (plusp branch-total) (zerop branch-covered))
                 (listp locations))
      (error "Malformed or empty cl-weave coverage statistics for ~S." pathname))
    (let* ((forms (top-level-forms source))
           (excluded
             (remove-if-not
              (lambda (form)
                (and (member (getf form :head) heads :test #'equal)
                     (find-if (lambda (location) (%spans-form-p location form))
                              locations)))
              forms))
           (unexcused
             (remove-if (lambda (location)
                          (some (lambda (form) (%spans-form-p location form))
                                excluded))
                        locations)))
      (list :file (enough-namestring (pathname pathname) (uiop:getcwd))
            :pathname pathname
            :expression-covered expression-covered
            :expression-total expression-total
            :branch-covered branch-covered
            :branch-total branch-total
            :expression-shortfall (- (- expression-total expression-covered)
                                     (length excluded))
            :branch-shortfall (- branch-total branch-covered)
            :excluded (mapcar (lambda (form)
                                (list :head (getf form :head)
                                      :line (1+ (count #\Newline source
                                                       :end (getf form :start)))))
                              excluded)
            :unexcused unexcused))))

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
  (reduce #'+ verdicts
          :key (lambda (verdict) (getf verdict kind))
          :initial-value 0))

(defun %allowance-mismatches (verdicts allowance)
  (let ((measured-keys (mapcar (lambda (verdict) (getf verdict :file)) verdicts))
        (allowed-keys (mapcar (lambda (entry) (getf entry :file)) allowance)))
    (append
     (mapcar (lambda (key) (list :reason :duplicate-measured-file :file key))
             (remove-duplicates
              (remove-if (lambda (key) (= 1 (count key measured-keys :test #'string=)))
                         measured-keys)
              :test #'string=))
     (mapcar (lambda (key) (list :reason :duplicate-allowance-file :file key))
             (remove-duplicates
              (remove-if (lambda (key) (= 1 (count key allowed-keys :test #'string=)))
                         allowed-keys)
              :test #'string=))
     (mapcan
      (lambda (verdict)
        (let* ((file (getf verdict :file))
               (entry (find file allowance :key (lambda (item) (getf item :file))
                            :test #'string=)))
          (cond ((null entry)
                 (list (list :reason :missing-allowance :file file)))
                ((or (/= (getf verdict :expression-shortfall)
                         (getf entry :expression))
                     (/= (getf verdict :branch-shortfall)
                         (getf entry :branch)))
                 (list (list :reason :shortfall-changed :file file
                             :actual-expression (getf verdict :expression-shortfall)
                             :allowed-expression (getf entry :expression)
                             :actual-branch (getf verdict :branch-shortfall)
                             :allowed-branch (getf entry :branch)))))))
      verdicts)
     (mapcan
      (lambda (entry)
        (unless (find (getf entry :file) verdicts
                      :key (lambda (verdict) (getf verdict :file))
                      :test #'string=)
          (list (list :reason :unmeasured-allowance :file (getf entry :file)))))
      allowance))))

(defun %gate-passed-p (verdicts rules allowance)
  (and verdicts
       (null (unused-rules verdicts rules))
       (null (%allowance-mismatches verdicts allowance))))

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
  "Print per-file coverage and exact shortfall mismatches, stale RULES, and PASS or FAIL."
  (format stream "~&file~Texpr~Tbranch~Texpression gap actual/allowed~Tbranch gap actual/allowed~%")
  (dolist (verdict verdicts)
    (let ((entry (find (getf verdict :file) allowance
                       :key (lambda (item) (getf item :file))
                       :test #'string=)))
      (format stream "~A~T~D/~D~T~D/~D~T~D/~A~T~D/~A~%"
              (getf verdict :file)
              (getf verdict :expression-covered) (getf verdict :expression-total)
              (getf verdict :branch-covered) (getf verdict :branch-total)
              (getf verdict :expression-shortfall)
              (if entry (getf entry :expression) "missing")
              (getf verdict :branch-shortfall)
              (if entry (getf entry :branch) "missing"))))
  (%write-locations verdicts stream)
  (dolist (rule (unused-rules verdicts rules))
    (format stream "stale exclusion: ~A excused nothing~%" (getf rule :head)))
  (dolist (mismatch (%allowance-mismatches verdicts allowance))
    (case (getf mismatch :reason)
      (:missing-allowance
       (format stream "missing allowance: ~A~%" (getf mismatch :file)))
      (:unmeasured-allowance
       (format stream "allowance has no measured file: ~A~%" (getf mismatch :file)))
      (:duplicate-measured-file
       (format stream "duplicate measured file: ~A~%" (getf mismatch :file)))
      (:duplicate-allowance-file
       (format stream "duplicate allowance file: ~A~%" (getf mismatch :file)))
      (:shortfall-changed
       (format stream "shortfall changed for ~A: expr actual/allowed ~D/~D, branch ~D/~D~%"
               (getf mismatch :file)
               (getf mismatch :actual-expression) (getf mismatch :allowed-expression)
               (getf mismatch :actual-branch) (getf mismatch :allowed-branch)))))
  (let ((expression (%total-shortfall verdicts :expression-shortfall))
        (branch (%total-shortfall verdicts :branch-shortfall))
        (allowed-expression (reduce #'+ allowance :key (lambda (entry) (getf entry :expression))
                                    :initial-value 0))
        (allowed-branch (reduce #'+ allowance :key (lambda (entry) (getf entry :branch))
                                :initial-value 0)))
    (format stream "shortfall totals: expr actual/allowed ~D/~D, branch ~D/~D~%"
            expression allowed-expression branch allowed-branch)
    (format stream "100% coverage: ~:[not met~;met~]~%"
            (and verdicts (zerop (+ expression branch)))))
  (format stream "coverage gate: ~:[FAIL~;PASS~]~%" (%gate-passed-p verdicts rules allowance)))

(defun gate-exit-status (verdicts rules allowance)
  "0 when VERDICTS exactly match every per-file allowance and all rules are used."
  (if (%gate-passed-p verdicts rules allowance) 0 1))
