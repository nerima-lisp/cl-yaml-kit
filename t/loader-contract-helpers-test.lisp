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

(defmacro loader-scalar-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (expect (yaml-kit::resolve-plain-scalar-tag
                             ,(third case) ,(second case))
                            :to-equal ,(fourth case))))
               cases)))
(defmacro loader-parse-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (expect (loader-parse-events
                             (loader-document
                              (loader-event :scalar :value ,(second case)))
                             ,@(third case))
                            :to-equal ,(fourth case))))
               cases)))

(defmacro loader-value-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (expect ,(second case) :to-equal ,(third case))))
               cases)))

(defmacro loader-predicate-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (expect (funcall ,(third case) ,(second case))
                            :to-be-truthy)))
               cases)))

(defmacro loader-error-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (expect (handler-case
                                (progn ,(second case) nil)
                              (,(third case) () t))
                            :to-be-truthy)))
               cases)))

(defmacro loader-limit-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (expect (handler-case
                                (progn ,(second case) nil)
                              (yaml-kit:yaml-resource-limit-error (condition)
                                (string= (yaml-kit::yaml-resource-limit-error-limit-name
                                          condition)
                                         ,(third case))))
                            :to-be-truthy)))
               cases)))

(defun loader-empty-collection-events (kind tag)
  (loader-document
   (if (eq kind :sequence)
       (loader-event :sequence-start :tag tag)
       (loader-event :mapping-start :tag tag))
   (if (eq kind :sequence)
       (loader-event :sequence-end)
       (loader-event :mapping-end))))

(defmacro loader-collection-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (expect (funcall ,(fourth case)
                                     (loader-parse-events
                                      (loader-empty-collection-events
                                       ,(second case) ,(third case))))
                            :to-be-truthy)))
               cases)))
