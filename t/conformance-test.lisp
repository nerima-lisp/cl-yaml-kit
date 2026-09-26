;;;; t/conformance-test.lisp
(in-package #:cl-yaml-kit/test)

;;;; The yaml-test-suite is a required fixture for this test system.  The
;;;; flake and direct runner both provide YAML_TEST_SUITE explicitly.

(defstruct conformance-case
  id name directory input event json out emit error)

(defvar *conformance-exclusions* nil)

(defparameter *conformance-stage-names*
  '(:reader :loader-isolated :loader-e2e :emitter-isolated :dumper-e2e))

(defvar *conformance-last-summaries* nil)

(defun conformance-source-root ()
  (handler-case
      (asdf:system-source-directory :cl-yaml-kit)
    (error ()
      (make-pathname :name nil :type nil
                     :defaults (or *load-truename* *compile-file-truename*
                                   *load-pathname*)))))

(defun conformance-suite-root ()
  (let* ((root (conformance-source-root))
         (configured (uiop:getenv "YAML_TEST_SUITE"))
         (candidates (if configured
                         (list (uiop:ensure-directory-pathname
                                (pathname configured)))
                         (list (merge-pathnames "yaml-test-suite/" root)
                               (merge-pathnames "yaml-test-suite-data/" root)
                               (merge-pathnames "data/yaml-test-suite/" root)))))
    (or (find-if #'uiop:directory-exists-p candidates)
        (error "YAML_TEST_SUITE must name an existing yaml-test-suite checkout"))))

(defun conformance-file-string (pathname)
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (with-output-to-string (out)
      (loop for character = (read-char stream nil nil)
            while character do (write-char character out)))))

(defun conformance-first-difference (expected actual &key (label "value"))
  (let ((expected-items (if (listp expected) expected (list expected)))
        (actual-items (if (listp actual) actual (list actual))))
    (loop for expected-item in expected-items
          for actual-item in actual-items
          for position from 0
          unless (equal expected-item actual-item)
            do (return (format nil "~A mismatch at position ~D: expected ~S, actual ~S"
                               label position expected-item actual-item))
          finally
             (unless (= (length expected-items) (length actual-items))
               (return (format nil "~A length mismatch: expected ~D, actual ~D"
                               label (length expected-items) (length actual-items)))))))

(defun conformance-lines (text)
  (uiop:split-string text :separator '(#\Newline)))

(defun conformance-first-line-difference (expected actual &key (label "text"))
  (let ((expected-lines (conformance-lines expected))
        (actual-lines (conformance-lines actual)))
    (or (loop for expected-line in expected-lines
              for actual-line in actual-lines
              for line-number from 1
              unless (string= expected-line actual-line)
                do (return (format nil "~A mismatch at line ~D: expected ~S, actual ~S"
                                   label line-number expected-line actual-line)))
        (unless (= (length expected-lines) (length actual-lines))
          (format nil "~A line count mismatch: expected ~D, actual ~D"
                  label (length expected-lines) (length actual-lines))))))

(defun conformance-file (directory name)
  (let ((pathname (merge-pathnames name directory)))
    (and (probe-file pathname) pathname)))

(defun conformance-case-from-directory (directory)
  (let ((id (car (last (pathname-directory directory)))))
    (make-conformance-case
     :id (string-downcase (princ-to-string id))
     :name (when (conformance-file directory "===")
             (string-trim '(#\Space #\Tab #\Newline #\Return)
                          (conformance-file-string
                           (conformance-file directory "==="))))
     :directory directory
     :input (conformance-file directory "in.yaml")
     :event (conformance-file directory "test.event")
     :json (conformance-file directory "in.json")
     :out (conformance-file directory "out.yaml")
     :emit (conformance-file directory "emit.yaml")
     :error (conformance-file directory "error"))))

(defun conformance-cases ()
  (let ((root (conformance-suite-root)))
    (sort (loop for directory in (directory (merge-pathnames "*/" root))
                when (and (uiop:directory-exists-p directory)
                          (conformance-file directory "in.yaml"))
                  collect (conformance-case-from-directory directory))
          #'string< :key #'conformance-case-id)))

(defun conformance-read-exclusions ()
  (let ((pathname (merge-pathnames "t/data/conformance-exclusions.lisp"
                                  (conformance-source-root))))
    (if (probe-file pathname)
        (progn (load pathname) *conformance-exclusions*)
        nil)))

(defun conformance-exclusion-valid-p (entry ids)
  (and (consp entry)
       (= (length entry) 3)
       (member (string-downcase (princ-to-string (car entry))) ids :test #'string=)
       (member (second entry) *conformance-stage-names*)
       (stringp (third entry))))

(defun conformance-exclusions-valid-p (cases exclusions)
  (let ((ids (mapcar #'conformance-case-id cases)))
    (and (listp exclusions)
         (= (length exclusions) (length (remove-duplicates exclusions :key #'car
                                                             :test #'equal)))
         (every (lambda (entry) (conformance-exclusion-valid-p entry ids))
                exclusions))))

(defun conformance-excluded-p (case stage exclusions)
  (member stage (cdr (assoc (conformance-case-id case) exclusions
                            :test #'string-equal))))

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
                       :failure-causes nil)))
    (dolist (case cases summary)
      (multiple-value-bind (applicable passedp condition)
          (conformance-stage-result case stage)
        (if (not applicable)
            (incf (getf summary :skipped))
            (progn
              (incf (getf summary :total))
              (cond
                ((conformance-excluded-p case stage exclusions)
                 (incf (getf summary :excluded))
                 (if passedp
                     (progn
                       (incf (getf summary :drift))
                       (push (conformance-case-id case) (getf summary :drift-ids)))
                     (progn
                       (incf (getf summary :failed))
                       (push (conformance-case-id case) (getf summary :failure-ids))
                       (push (list (conformance-case-id case)
                                   (princ-to-string condition))
                             (getf summary :failure-causes)))))
                (passedp (incf (getf summary :passed)))
                (t
                 (incf (getf summary :failed))
                 (push (conformance-case-id case) (getf summary :failure-ids))
                 (push (list (conformance-case-id case)
                             (princ-to-string condition))
                       (getf summary :failure-causes))))))))))

(defun conformance-cause-category (condition)
  (let ((text (string-downcase (or condition ""))))
    (cond ((search "event" text) :event-conversion)
          ((or (search "compose" text) (search "construct" text)
               (search "yaml" text)) :loader-or-schema)
          ((or (search "emit" text) (search "stream" text)) :emitter-format)
          ((search "json" text) :expected-value)
          (t :other))))

(defun conformance-write-status (summaries)
  (let ((pathname (merge-pathnames ".mediator/research/conformance-status.md"
                                   (conformance-source-root))))
    (ensure-directories-exist pathname)
    (with-open-file (stream pathname :direction :output :if-exists :supersede
                            :if-does-not-exist :create :external-format :utf-8)
      (format stream "# Conformance status~%~%")
      (format stream "Generated by the conformance test run. Re-run with the command in `docs/src/project/development.md`.~%~%")
      (format stream "| stage | applicable | passed | failed | excluded | drift |~%|---|---:|---:|---:|---:|---:|~%")
      (dolist (summary summaries)
        (format stream "| ~A | ~D | ~D | ~D | ~D | ~D |~%"
                (getf summary :stage) (getf summary :total)
                (getf summary :passed) (getf summary :failed)
                (getf summary :excluded) (getf summary :drift)))
      (dolist (summary summaries)
        (format stream "~%## ~A failures~%~%IDs: ~{~A~^, ~}~%"
                (getf summary :stage)
                (sort (copy-list (getf summary :failure-ids)) #'string<))
        (when (member (getf summary :stage) '(:loader-isolated :emitter-isolated))
          (let ((groups (make-hash-table)))
            (dolist (failure (getf summary :failure-causes))
              (push (first failure) (gethash (conformance-cause-category (second failure) ) groups)))
            (maphash (lambda (category ids)
                     (format stream "~%### ~A~%~{~A~^, ~}~%"
                               category (sort ids #'string<)))
                     groups)))))))

(defun conformance-run-report ()
  (let* ((cases (conformance-cases))
         (exclusions (conformance-read-exclusions))
         (summaries (mapcar (lambda (stage)
                              (conformance-stage-summary cases stage exclusions))
                            *conformance-stage-names*)))
    (conformance-write-status summaries)
    (dolist (summary summaries)
      (format t "~&conformance ~A: applicable=~D passed=~D failed=~D excluded=~D drift=~D~%"
              (getf summary :stage) (getf summary :total)
              (getf summary :passed) (getf summary :failed)
              (getf summary :excluded) (getf summary :drift)))
    (setf *conformance-last-summaries* summaries)
    (values summaries
            (every (lambda (summary)
                    (and (zerop (getf summary :failed))
                         (zerop (getf summary :drift))))
                   summaries))))

(describe "yaml-test-suite conformance harness"
  (it "keeps fixture and exclusion metadata loadable"
    (let ((cases (conformance-cases))
          (exclusions (conformance-read-exclusions)))
      (expect (conformance-exclusions-valid-p cases exclusions) :to-be-truthy)
      (expect (equal '((:stream-start) (:document-start t) (:scalar nil nil #\: "hello")
                       (:document-end nil) (:stream-end))
                     (conformance-event-signatures
                      (format nil "+STR~%+DOC ---~%=VAL :hello~%-DOC~%-STR~%")))
              :to-be-truthy)))
  (it "gates reader, loader, dumper, and emitter stages"
    (multiple-value-bind (summaries passedp)
        (conformance-run-report)
      (expect (length summaries) :to-be 5)
      (expect passedp
              :to-be-truthy))))
