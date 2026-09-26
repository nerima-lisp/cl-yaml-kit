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
  (json-kit:parse (conformance-file-string pathname)
                  :object-type :hash-table
                  :array-type :vector))

(defun conformance-loader-value (case)
  (yaml-kit:parse-all (conformance-file-string
                       (conformance-case-input case))))

(defun conformance-values-equal-p (actual expected)
  (equal (conformance-canonical-value actual)
         (conformance-canonical-value expected)))

(defun conformance-loader-result (case)
  (handler-case
      (let ((actual (conformance-loader-value case))
            (expected (conformance-json-value
                       (conformance-case-json case))))
        (values (conformance-values-equal-p actual (list expected)) nil))
    (error (condition) (values nil condition))))

(defun conformance-output-result (case)
  (handler-case
      (let* ((value (conformance-loader-value case))
             (emitted (yaml-kit:emit value))
             (actual (yaml-kit:parse-all emitted))
             (expected (yaml-kit:parse-all
                        (conformance-file-string
                         (conformance-case-output case)))))
        (values (and (conformance-values-equal-p actual value)
                     (conformance-values-equal-p actual expected)) nil))
    (error (condition) (values nil condition))))

(defun conformance-emitter-result (case)
  (handler-case
      (let* ((value (conformance-loader-value case))
             (emitted (yaml-kit:emit value))
             (expected (conformance-file-string
                        (conformance-case-emit case))))
        (values (string= emitted expected) nil))
    (error (condition) (values nil condition))))
