(in-package #:cl-yaml-kit/test)

(defun conformance-reader-result (case)
  (handler-case
      (let ((events nil))
        (yaml-kit:map-events
         (lambda (event) (push event events))
         (conformance-file-string (conformance-case-input case)))
        (setf events (nreverse events))
        (let* ((actual (mapcar #'conformance-event-signature events))
               (expected (conformance-event-signatures
                          (conformance-file-string
                           (conformance-case-event case))))
               (passed (and (not (conformance-case-error case))
                            (conformance-case-event case)
                            (equal actual expected))))
          (values passed
                  (unless passed
                    (conformance-first-difference expected actual
                                                   :label "event")))))
    (error (condition) (values (and (conformance-case-error case) t) condition))))

(defun conformance-load-result (case)
  (handler-case
      (values (funcall (symbol-function 'yaml-kit:parse)
                       (conformance-file-string (conformance-case-input case)))
              t)
    (error (condition) (values nil condition))))

(defun conformance-dump-result (value)
  (handler-case
      (values (funcall (symbol-function 'yaml-kit:emit) value) t)
    (error (condition) (values nil condition))))

(defun conformance-stage-result (case stage)
  (handler-case
      (case stage
        (:reader
         (when (or (conformance-case-event case) (conformance-case-error case))
           (multiple-value-bind (passed condition) (conformance-reader-result case)
             (values t passed condition))))
        (:loader-isolated
         (when (and (conformance-case-event case) (conformance-case-json case))
           (multiple-value-bind (passed condition) (conformance-loader-isolated-result case)
             (values t passed condition))))
        (:loader-e2e
         (when (conformance-case-json case)
           (multiple-value-bind (passed condition) (conformance-loader-e2e-result case)
             (values t passed condition))))
        (:dumper-e2e
         (when (conformance-case-out case)
           (multiple-value-bind (passed condition) (conformance-dumper-e2e-result case)
             (values t passed condition))))
        (:emitter-isolated
         (when (and (conformance-case-event case) (conformance-case-emit case))
           (multiple-value-bind (passed condition) (conformance-emitter-isolated-result case)
             (values t passed condition)))))
    (error (condition)
      (values t nil condition))))

(defun conformance-stage-summary (cases stage exclusions)
  (let ((summary (list :stage stage :total 0 :passed 0 :failed 0 :skipped 0
                       :excluded 0 :drift 0 :failure-ids nil :drift-ids nil
                       :failure-causes nil :failure-details nil)))
    (dolist (case cases summary)
      (multiple-value-bind (applicable passedp condition)
          (conformance-stage-result case stage)
        (if (not applicable)
            (incf (getf summary :skipped))
            (progn
              (incf (getf summary :total))
              (cond
                ((and (eq passedp :skipped)
                      (not (conformance-excluded-p case stage exclusions)))
                 (incf (getf summary :skipped)))
                ((conformance-excluded-p case stage exclusions)
                 (incf (getf summary :excluded))
                 (unless (eq passedp :skipped)
                   (if passedp
                       (progn
                         (incf (getf summary :drift))
                         (push (conformance-case-id case) (getf summary :drift-ids)))
                       (progn
                         (incf (getf summary :failed))
                         (push (conformance-case-id case) (getf summary :failure-ids))
                         (push (list (conformance-case-id case) condition)
                               (getf summary :failure-causes))
                         (push (list :stage stage :id (conformance-case-id case)
                                     :detail (princ-to-string condition))
                               (getf summary :failure-details))))))
                (passedp (incf (getf summary :passed)))
                (t
                 (incf (getf summary :failed))
                 (push (conformance-case-id case) (getf summary :failure-ids))
                 (push (list (conformance-case-id case) condition)
                       (getf summary :failure-causes))
                 (push (list :stage stage :id (conformance-case-id case)
                             :detail (princ-to-string condition))
                       (getf summary :failure-details))))))))))
