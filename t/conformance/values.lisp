(in-package #:cl-yaml-kit/test)

;;;; JSON is used only as the yaml-test-suite's expected-value syntax.  The
;;;; canonical form below keeps JSON null/false distinct from NIL and keeps
;;;; integers distinct from floating point numbers.

(defun conformance-canonical-value (value)
  (cond
    ((yaml-kit:yaml-null-p value) '(:yaml-null))
    ((yaml-kit:yaml-false-p value) '(:yaml-false))
    ((json-kit:json-null-p value) '(:yaml-null))
    ((json-kit:json-false-p value) '(:yaml-false))
    ((null value) '(:lisp-nil))
    ((integerp value) (list :integer value))
    ((floatp value) (list :float value))
    ((stringp value) (list :string value))
    ((characterp value) (list :character value))
    ((hash-table-p value)
     (list :object
           (sort (loop for key being the hash-keys of value using (hash-value item)
                       collect (cons (conformance-canonical-value key)
                                     (conformance-canonical-value item)))
                 #'string< :key (lambda (entry) (prin1-to-string (car entry))))))
    ((yaml-kit:yaml-mapping-p value)
     (list :object
           (sort (mapcar (lambda (entry)
                           (cons (conformance-canonical-value (car entry))
                                 (conformance-canonical-value (cdr entry))))
                         (yaml-kit:yaml-mapping-entries value))
                 #'string< :key (lambda (entry) (prin1-to-string (car entry))))))
    ((vectorp value)
     (list :array (map 'list #'conformance-canonical-value value)))
    ((listp value)
     (list :array (mapcar #'conformance-canonical-value value)))
    (t (list :other (princ-to-string value)))))

(defun conformance-json-value (pathname)
  (let ((text (conformance-file-string pathname))
        (index 0)
        (values nil))
    (loop
      (loop while (and (< index (length text))
                       (find (char text index)
                             '(#\Space #\Tab #\Return #\Newline)))
            do (incf index))
      (if (= index (length text))
          (return (nreverse values))
          (multiple-value-bind (value end-index)
              (json-kit:parse-prefix text :index index
                                     :object-type :hash-table
                                     :array-type :vector)
            (push value values)
            (setf index end-index))))))

(defun conformance-loader-value (case)
  (yaml-kit:parse-all (conformance-file-string
                       (conformance-case-input case))))

(defun conformance-values-equal-p (actual expected)
  (equal (conformance-canonical-value actual)
         (conformance-canonical-value expected)))

(defun conformance-loader-difference (expected actual)
  (let ((expected-items (if (listp expected) expected (list expected)))
        (actual-items (if (listp actual) actual (list actual))))
    (loop for position from 0
          for expected-item in expected-items
          for actual-item in actual-items
          for expected-canonical = (conformance-canonical-value expected-item)
          for actual-canonical = (conformance-canonical-value actual-item)
          unless (equal expected-canonical actual-canonical)
            do (return (format nil
                               "loader value mismatch at position ~D: expected ~S, actual ~S"
                               position expected-canonical actual-canonical))
          finally
             (unless (= (length expected-items) (length actual-items))
               (let ((position (min (length expected-items)
                                    (length actual-items))))
                 (return (format nil
                                 "loader value mismatch at position ~D: expected ~S, actual ~S"
                                 position
                                 (if (< position (length expected-items))
                                     (conformance-canonical-value
                                      (nth position expected-items))
                                     :missing)
                                 (if (< position (length actual-items))
                                     (conformance-canonical-value
                                      (nth position actual-items))
                                     :missing))))))))

(defun conformance-emitter-difference (expected actual)
  (conformance-first-line-difference expected actual :label "emitter"))

(defun conformance-loader-isolated-result (case)
  (handler-case
      (let* ((events (conformance-events
                      (conformance-file-string (conformance-case-event case))))
             (actual (yaml-kit:parse-all events))
             (expected (conformance-json-value
                        (conformance-case-json case))))
        (let ((passed (conformance-values-equal-p actual expected)))
          (values passed
                  (unless passed
                    (conformance-loader-difference expected actual)))))
    (error (condition) (values nil condition))))

(defun conformance-loader-e2e-result (case)
  (handler-case
      (let ((actual (conformance-loader-value case))
            (expected (conformance-json-value
                       (conformance-case-json case))))
        (let ((passed (conformance-values-equal-p actual expected)))
          (values passed
                  (unless passed
                    (conformance-loader-difference expected actual)))))
    (error (condition) (values nil condition))))

(defun conformance-dumper-e2e-result (case)
  (handler-case
      (let* ((values (handler-case
                         (conformance-loader-value case)
                       (error (condition)
                         (return-from conformance-dumper-e2e-result
                           (values t condition)))))
             (roundtrip-values
               (mapcar (lambda (value)
                         (let ((documents
                                 (yaml-kit:parse-all (yaml-kit:emit value))))
                           (unless (= (length documents) 1)
                             (error "dumper-e2e emitted ~D documents for one input value"
                                    (length documents)))
                           (car documents)))
                       values))
             (value-column (if values values :missing))
             (actual-column (if roundtrip-values roundtrip-values :missing)))
        (let ((passed (conformance-values-equal-p actual-column value-column)))
          (values passed
                  (unless passed
                    (conformance-loader-difference value-column actual-column)))))
    (error (condition) (values nil condition))))

(defun conformance-emitter-isolated-result (case)
  (handler-case
      (let* ((events (conformance-events
                      (conformance-file-string (conformance-case-event case))))
             (emitted (yaml-kit:emit-events events))
             (expected (conformance-file-string
                        (conformance-case-emit case))))
        (let ((passed (string= emitted expected)))
          (values passed
                  (unless passed
                    (conformance-emitter-difference expected emitted)))))
    (error (condition) (values nil condition))))
