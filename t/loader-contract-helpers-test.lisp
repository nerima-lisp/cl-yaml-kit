(in-package #:cl-yaml-kit/test)

(defun loader-event (type &rest options)
  (apply (ecase type
           (:stream-start #'yaml-kit:make-stream-start-event)
           (:stream-end #'yaml-kit:make-stream-end-event)
           (:document-start #'yaml-kit:make-document-start-event)
           (:document-end #'yaml-kit:make-document-end-event)
           (:scalar #'yaml-kit:make-scalar-event)
           (:sequence-start #'yaml-kit:make-sequence-start-event)
           (:sequence-end #'yaml-kit:make-sequence-end-event)
           (:mapping-start #'yaml-kit:make-mapping-start-event)
           (:mapping-end #'yaml-kit:make-mapping-end-event)
           (:alias #'yaml-kit:make-alias-event))
         options))

(defun loader-document (&rest events)
  (append (list (loader-event :stream-start)
                (loader-event :document-start))
          events
          (list (loader-event :document-end)
                (loader-event :stream-end))))

(defun loader-compose-events (events &rest options)
  (apply #'yaml-kit:compose-events
         (cons (yaml-kit::event-list-source events) options)))

(defun loader-compose-all-events (events &rest options)
  (apply #'yaml-kit:compose-all-events
         (cons (yaml-kit::event-list-source events) options)))

(defun loader-parse-events (events &key (schema :core) (mapping-type :hash-table)
                                      (sequence-type :vector)
                                      (duplicate-key-policy :error)
                                      (max-depth 1000)
                                      (max-scalar-length 16777216)
                                      (max-nodes 1000000)
                                      (max-alias-expansions 100000))
  (yaml-kit::construct
   (loader-compose-events
    events :max-depth max-depth
    :max-scalar-length max-scalar-length :max-nodes max-nodes
    :max-alias-expansions max-alias-expansions)
   :schema schema :mapping-type mapping-type :sequence-type sequence-type
   :duplicate-key-policy duplicate-key-policy))

(defun loader-parse-all-events (events &key (schema :core) (mapping-type :hash-table)
                                          (sequence-type :vector)
                                          (duplicate-key-policy :error)
                                          (max-depth 1000)
                                          (max-scalar-length 16777216)
                                          (max-nodes 1000000)
                                          (max-alias-expansions 100000))
  (let ((values nil))
    (loader-compose-all-events
     events :max-depth max-depth
     :max-scalar-length max-scalar-length :max-nodes max-nodes
     :max-alias-expansions max-alias-expansions
     :document-handler
     (lambda (node)
       (push (yaml-kit::construct
              node :schema schema :mapping-type mapping-type
              :sequence-type sequence-type
              :duplicate-key-policy duplicate-key-policy)
             values)))
    (nreverse values)))

(defun loader-empty-collection-events (kind tag)
  (loader-document
   (if (eq kind :sequence)
       (loader-event :sequence-start :tag tag)
       (loader-event :mapping-start :tag tag))
   (if (eq kind :sequence)
       (loader-event :sequence-end)
       (loader-event :mapping-end))))

(progn
  (defun loader-case-events (specifications)
    (mapcar (lambda (specification)
              (if (consp specification)
                  (apply #'loader-event specification)
                  specification))
            specifications))

  (defun loader-case-call (operation input options)
    (ecase operation
      (:compose-events
       (apply #'loader-compose-events
              (cons (loader-case-events input) options)))
      (:compose-all-events
       (apply #'loader-compose-all-events
              (cons (loader-case-events input) options)))
      (:parse-events
       (apply #'loader-parse-events
              (cons (loader-case-events input) options)))
      (:compose-document
       (apply #'loader-compose-events
              (cons (apply #'loader-document (loader-case-events input))
                    options)))
      (:parse-document
       (apply #'loader-parse-events
              (cons (apply #'loader-document (loader-case-events input))
                    options)))
      (:parse (apply #'yaml-kit:parse (cons input options)))
      (:parse-all (apply #'yaml-kit:parse-all (cons input options)))
      (:schema-table (yaml-kit::%schema-table input))
      (:construct (apply #'yaml-kit::construct (cons input options)))
      (:limit!
       (apply (symbol-function 'yaml-kit::%limit!)
              (append input options)))))


  (cl-weave:defmatcher :to-signal-loader-error (actual expected)
    (let ((condition (handler-case (funcall actual)
                       (error (condition) condition))))
      (multiple-value-bind (expected-type valid-p)
          (%yaml-single-matcher-value expected)
        (values (and condition valid-p (typep condition expected-type))
                condition
                (if valid-p expected-type expected)))))

  (cl-weave:defmatcher :to-signal-loader-compose-cause (actual expected)
    (let ((condition (handler-case (funcall actual)
                       (yaml-kit:yaml-compose-error (condition) condition))))
      (multiple-value-bind (expected-cause valid-p)
          (%yaml-single-matcher-value expected)
        (values (and condition valid-p (stringp expected-cause)
                     (string= (yaml-kit:yaml-compose-error-cause condition)
                              expected-cause))
                condition
                (if valid-p expected-cause expected)))))

  (cl-weave:defmatcher :to-signal-loader-resource-limit (actual expected)
    (let ((condition (handler-case (funcall actual)
                       (yaml-kit:yaml-resource-limit-error (condition) condition))))
      (multiple-value-bind (limits valid-p)
          (%yaml-single-matcher-value expected)
        (let* ((valid-limits-p (and valid-p (listp limits)
                                    (evenp (length limits))))
               (expected-name (and valid-limits-p (getf limits :name))))
          (values (and condition valid-limits-p (stringp expected-name)
                       (string= (yaml-kit::yaml-resource-limit-error-limit-name condition)
                                expected-name)
                       (or (not (member :limit limits))
                           (equal (yaml-kit::yaml-resource-limit-error-limit condition)
                                  (getf limits :limit)))
                       (or (not (member :actual limits))
                           (equal (yaml-kit::yaml-resource-limit-error-actual condition)
                                  (getf limits :actual))))
                  condition
                  (if valid-p limits expected)))))))
