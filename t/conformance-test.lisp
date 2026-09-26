;;;; t/conformance-test.lisp
(in-package #:cl-yaml-kit/test)

;;;; The yaml-test-suite is a required fixture for this test system.  The
;;;; flake and direct runner both provide YAML_TEST_SUITE explicitly.

(defstruct conformance-case
  id name directory input event json out emit error)

(defparameter *conformance-stage-names* '(:reader :loader :dumper :emitter))

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
                         (list (pathname configured))
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

(defun conformance-unescape (text)
  (with-output-to-string (out)
    (loop for i from 0 below (length text)
          for character = (char text i)
          do (if (and (char= character #\\) (< (1+ i) (length text)))
                 (progn
                   (incf i)
                   (write-char (case (char text i)
                                 (#\n #\Newline) (#\t #\Tab) (#\r #\Return)
                                 (#\b #\Backspace) (otherwise (char text i))) out))
                 (write-char character out)))))

(defun conformance-bracket-fields (text)
  (loop with position = 0
        while (and (< position (length text))
                   (char= (char text position) #\Space))
        do (incf position)
        when (and (< position (length text))
                  (char= (char text position) #\[))
          collect (let ((end (or (position #\] text :start position)
                                 (error "Malformed conformance event field: ~S" text))))
                    (prog1 (subseq text (1+ position) end)
                      (setf position (1+ end))))))

(defun conformance-event-line (line)
  (let* ((text (string-trim '(#\Space #\Tab #\Return) line))
         (kind (subseq text 0 (min 4 (length text)))))
    (cond
      ((string= kind "+STR") '(:stream-start))
      ((string= kind "-STR") '(:stream-end))
      ((string= kind "+DOC") (list :document-start (not (null (search "---" text)))))
      ((string= kind "-DOC") (list :document-end (not (null (search "..." text)))))
      ((or (string= kind "+SEQ") (string= kind "+MAP"))
       (let ((fields (conformance-bracket-fields (subseq text 4))))
         (list (if (char= (char kind 1) #\S) :sequence-start :mapping-start)
               (not (null (search (if (char= (char kind 1) #\S) "[]" "{}") text)))
               (and (first (rest fields))
                    (subseq (first (rest fields)) 1))
               (and (second (rest fields))
                    (subseq (second (rest fields)) 1
                            (1- (length (second (rest fields)))))))))
      ((or (string= kind "-SEQ") (string= kind "-MAP"))
       (list (if (char= (char kind 1) #\S) :sequence-end :mapping-end)))
      ((string= kind "=ALI")
       (list :alias (string-left-trim '(#\Space #\*) (subseq text 4))))
      ((string= kind "=VAL")
       (let* ((rest (subseq text 4))
              (fields (conformance-bracket-fields rest))
              (start (or (position-if (lambda (character)
                                        (member character '(#\: #\' #\" #\| #\>)))
                                      rest)
                          (length rest)))
              (value (string-left-trim '(#\Space) (subseq rest start))))
         (list :scalar (first fields) (second fields)
               (when (plusp (length value)) (char value 0))
               (conformance-unescape (if (plusp (length value))
                                         (subseq value 1) "")))))
      (t (error "Unknown conformance event line: ~S" line)))))

(defun conformance-event-signatures (text)
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        unless (zerop (length (string-trim '(#\Space #\Tab #\Return) line)))
          collect (conformance-event-line line)))

(defun conformance-event (event)
  (cond
    ((yaml-kit:stream-start-event-p event) '(:stream-start))
    ((yaml-kit:stream-end-event-p event) '(:stream-end))
    ((yaml-kit:document-start-event-p event)
     (list :document-start (yaml-kit:document-start-event-explicit-p event)))
    ((yaml-kit:document-end-event-p event)
     (list :document-end (yaml-kit:document-end-event-explicit-p event)))
    ((yaml-kit:sequence-start-event-p event)
     (list :sequence-start (eq (yaml-kit:sequence-start-event-style event) :flow)
           (yaml-kit:sequence-start-event-anchor event)
           (yaml-kit:sequence-start-event-tag event)))
    ((yaml-kit:mapping-start-event-p event)
     (list :mapping-start (eq (yaml-kit:mapping-start-event-style event) :flow)
           (yaml-kit:mapping-start-event-anchor event)
           (yaml-kit:mapping-start-event-tag event)))
    ((yaml-kit:sequence-end-event-p event) '(:sequence-end))
    ((yaml-kit:mapping-end-event-p event) '(:mapping-end))
    ((yaml-kit:alias-event-p event)
     (list :alias (yaml-kit:alias-event-anchor event)))
    ((yaml-kit:scalar-event-p event)
     (list :scalar (yaml-kit:scalar-event-anchor event)
           (yaml-kit:scalar-event-tag event)
           (case (yaml-kit:scalar-event-style event)
             (:plain #\:) (:single-quoted #\') (:double-quoted #\")
             (:literal #\|) (:folded #\>) (otherwise #\:))
           (yaml-kit:scalar-event-value event)))))

(defun conformance-reader-result (case)
  (handler-case
      (let ((events nil))
        (yaml-kit:map-events
         (lambda (event) (push event events))
         (conformance-file-string (conformance-case-input case)))
        (setf events (nreverse events))
        (values (if (conformance-case-error case)
                    nil
                    (and (conformance-case-event case)
                         (equal (mapcar #'conformance-event events)
                                (conformance-event-signatures
                                 (conformance-file-string
                                  (conformance-case-event case))))))
                nil))
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
  (case stage
    (:reader
     (when (or (conformance-case-event case) (conformance-case-error case))
       (multiple-value-bind (passed condition) (conformance-reader-result case)
         (values t passed condition))))
    (:loader
     (when (conformance-case-json case)
       (multiple-value-bind (passed condition) (conformance-loader-result case)
         (values t passed condition))))
    (:dumper
     (when (conformance-case-out case)
       (multiple-value-bind (passed condition) (conformance-output-result case)
         (values t passed condition))))
    (:emitter
     (when (conformance-case-emit case)
       (multiple-value-bind (passed condition) (conformance-emitter-result case)
         (values t passed condition))))))

(defun conformance-stage-summary (cases stage exclusions)
  (let ((summary (list :stage stage :total 0 :passed 0 :failed 0 :skipped 0
                       :excluded 0 :drift 0)))
    (dolist (case cases summary)
      (incf (getf summary :total))
      (multiple-value-bind (applicable passedp condition)
          (conformance-stage-result case stage)
        (declare (ignore condition))
        (cond
          ((not applicable) (incf (getf summary :skipped)))
          ((conformance-excluded-p case stage exclusions)
           (incf (getf summary :excluded))
           (if passedp
               (incf (getf summary :drift))
               (incf (getf summary :skipped))))
          (passedp (incf (getf summary :passed)))
          (t (incf (getf summary :failed)))))))

(describe "yaml-test-suite conformance harness"
  (it "keeps fixture and exclusion metadata loadable"
    (let ((cases (conformance-cases))
          (exclusions (conformance-read-exclusions)))
      (expect (conformance-exclusions-valid-p cases exclusions) :to-be-truthy)
      (expect (listp (conformance-event-signatures "+STR\n+DOC [---]\n=VAL :hello\n-DOC\n-STR\n"))
              :to-be-truthy)))
  (it "gates reader, loader, dumper, and emitter stages"
    (let* ((cases (conformance-cases))
           (exclusions (conformance-read-exclusions))
           (summaries (mapcar (lambda (stage)
                                (conformance-stage-summary cases stage exclusions))
                              *conformance-stage-names*)))
      (expect (length summaries) :to-be 4)
      (expect (every (lambda (summary)
                       (and (zerop (getf summary :failed))
                            (zerop (getf summary :drift))))
                     summaries)
              :to-be-truthy)))))
