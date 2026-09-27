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
  )
