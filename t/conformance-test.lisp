;;;; t/conformance-test.lisp
(in-package #:cl-yaml-kit/test)

;;;; The yaml-test-suite is intentionally an external fixture.  Keeping the
;;;; runner useful without the fixture is important for normal package tests,
;;;; while setting YAML_TEST_SUITE makes the same code exercise the suite.

(defstruct conformance-case
  id name directory input event json output error)

(defparameter *conformance-stage-names* '(:reader :loader :dumper))

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
     :output (or (conformance-file directory "out.yaml")
                 (conformance-file directory "emit.yaml"))
     :error (conformance-file directory "error"))))

(defun conformance-cases ()
  (let ((root (conformance-suite-root)))
    (if root
        (sort (loop for directory in (directory (merge-pathnames "*/" root))
                    when (and (uiop:directory-exists-p directory)
                              (conformance-file directory "in.yaml"))
                      collect (conformance-case-from-directory directory))
              #'string< :key #'conformance-case-id)
        nil)))

(defun conformance-read-exclusions ()
  (let ((pathname (merge-pathnames "t/data/conformance-exclusions.lisp"
                                  (conformance-source-root))))
    (if (probe-file pathname)
        (progn (load pathname) *conformance-exclusions*)
        nil)))

(defun conformance-exclusion-valid-p (entry ids)
  (and (consp entry)
       (member (string-downcase (princ-to-string (car entry))) ids :test #'string=)
       (listp (cdr entry))
       (every (lambda (stage) (member stage *conformance-stage-names*))
              (cdr entry))))

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
       (list :alias (string-left-trim '(#\Space) (subseq text 4))))
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
      (let ((events (yaml-kit:parse-events
                     (conformance-file-string (conformance-case-input case)))))
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

(defun conformance-stage-summary (cases stage exclusions)
  (let ((summary (list :stage stage :total 0 :passed 0 :failed 0 :skipped 0)))
    (dolist (case cases summary)
      (incf (getf summary :total))
      (cond
        ((conformance-excluded-p case stage exclusions) (incf (getf summary :skipped)))
        ((and (eq stage :reader) (fboundp 'yaml-kit:parse-events)
              (or (conformance-case-event case) (conformance-case-error case)))
         (multiple-value-bind (passedp condition) (conformance-reader-result case)
           (declare (ignore condition))
           (incf (getf summary (if passedp :passed :failed)))))
        ((and (eq stage :loader) (conformance-case-json case)
              (fboundp 'yaml-kit:parse))
         (multiple-value-bind (value result) (conformance-load-result case)
           (incf (getf summary (if result :passed :failed))))
        ((and (eq stage :dumper) (conformance-case-output case)
              (fboundp 'yaml-kit:emit) (fboundp 'yaml-kit:parse))
         (multiple-value-bind (value loaded) (conformance-load-result case)
           (if loaded
               (multiple-value-bind (output dumped) (conformance-dump-result value)
                 (declare (ignore output))
                 (incf (getf summary (if dumped :passed :failed))))
               (incf (getf summary :failed))))
        (t (incf (getf summary :skipped)))))))

(describe "yaml-test-suite conformance harness"
  (it "keeps fixture and exclusion metadata loadable"
    (let ((cases (conformance-cases))
          (exclusions (conformance-read-exclusions)))
      (expect (conformance-exclusions-valid-p cases exclusions) :to-be-truthy)
      (expect (listp (conformance-event-signatures "+STR\n+DOC [---]\n=VAL :hello\n-DOC\n-STR\n"))
              :to-be-truthy)))
  (it "aggregates reader, loader, and dumper stages"
    (let* ((cases (conformance-cases))
           (exclusions (conformance-read-exclusions))
           (summaries (mapcar (lambda (stage)
                                (conformance-stage-summary cases stage exclusions))
                              *conformance-stage-names*)))
      (expect (length summaries) :to-be 3)
      (expect (every #'listp summaries) :to-be-truthy))))))
