(in-package #:cl-yaml-kit/test)

(defun continuation-entry-source (handler)
  (yaml-kit:map-events handler "value"))

(defun continuation-entry-scalar-node-p (value)
  (and (yaml-kit:scalar-node-p value)
       (string= (yaml-kit:scalar-node-value value) "value")))

(defun continuation-entry-value-matches-p (entry value)
  (case entry
    (:comment-spans/k (equal value '((0 . 2))))
    (:map-events/k (null value))
    (:parse-events/k
     (and (= (length value) 5)
          (yaml-kit:stream-start-event-p (first value))
          (yaml-kit:document-start-event-p (second value))
          (yaml-kit:scalar-event-p (third value))
          (yaml-kit:document-end-event-p (fourth value))
          (yaml-kit:stream-end-event-p (fifth value))))
    ((:compose-events/k :compose/k) (continuation-entry-scalar-node-p value))
    ((:compose-all-events/k :compose-all/k)
     (and (= (length value) 1) (continuation-entry-scalar-node-p (first value))))
    (:parse/k (string= value "value"))
    (:parse-all/k (equal value '("first" "second")))
    (:read-yaml/k (string= value "value"))
    ((:emit-events/k :emit/k) (and (stringp value) (search "value" value)))
    (:write-yaml/k (string= value "value"))
    (:edit-source/k (string= value (format nil "a: 2~%")))
    (:%scanner-next-token/cps
     (and value (eq (yaml-kit:token-kind value) :stream-start)))
    (:%parser-run/cps (null value))
    (:%composer-compose-all-events/cps
     (and (= (length value) 1) (continuation-entry-scalar-node-p (first value))))
    (:%constructor-construct/cps (string= value "value"))
    (:%representer-represent/cps (continuation-entry-scalar-node-p value))
    (:%serializer-serialize/cps (null value))
    (:%emitter-emit-event-stream/cps (null value))))

(cl-weave:it-each
 ((comment-spans-k :comment-spans/k) (map-events-k :map-events/k)
  (parse-events-k :parse-events/k)
  (compose-events-k :compose-events/k)
  (compose-all-events-k :compose-all-events/k)
  (compose-k :compose/k)
  (compose-all-k :compose-all/k)
  (parse-k :parse/k)
  (parse-all-k :parse-all/k)
  (read-yaml-k :read-yaml/k)
  (emit-events-k :emit-events/k)
  (emit-k :emit/k)
  (write-yaml-k :write-yaml/k)
  (edit-source-k :edit-source/k)
  (scanner-next-token-cps :%scanner-next-token/cps)
  (parser-run-cps :%parser-run/cps)
  (composer-compose-all-events-cps :%composer-compose-all-events/cps)
  (constructor-construct-cps :%constructor-construct/cps)
  (representer-represent-cps :%representer-represent/cps)
  (serializer-serialize-cps :%serializer-serialize/cps)
  (emitter-emit-event-stream-cps :%emitter-emit-event-stream/cps))
 "case ~S calls its continuation entry once with its specified value"
 (name entry)
 (declare (ignore name))
 (let ((callback-count 0)
       (callback-value :not-called)
       (callback-return nil)
       (handler-event-count 0))
   (flet ((capture (value)
            (incf callback-count)
            (setf callback-value value)
            :callback-result))
     (setf callback-return (ecase entry
                             (:comment-spans/k
                              (yaml-kit:comment-spans/k "#x" #'capture))
                             (:map-events/k
                              (yaml-kit:map-events/k
                               (lambda (event)
                                 (declare (ignore event))
                                 (incf handler-event-count))
                               "value"
                               #'capture))
                             (:parse-events/k
                              (yaml-kit:parse-events/k "value" #'capture))
                             (:compose-events/k
                              (yaml-kit:compose-events/k
                               #'continuation-entry-source
                               #'capture))
                             (:compose-all-events/k
                              (yaml-kit:compose-all-events/k
                               #'continuation-entry-source
                               #'capture))
                             (:compose/k (yaml-kit:compose/k "value" #'capture))
                             (:compose-all/k
                              (yaml-kit:compose-all/k
                               "value"
                               #'capture))
                             (:parse/k (yaml-kit:parse/k "value" #'capture))
                             (:parse-all/k
                              (yaml-kit:parse-all/k
                               (format nil "first~%---~%second")
                               #'capture))
                             (:read-yaml/k
                              (with-input-from-string (stream "value")
                                (yaml-kit:read-yaml/k stream #'capture)))
                             (:emit-events/k
                              (yaml-kit:emit-events/k
                               (yaml-kit:parse-events "value")
                               #'capture))
                             (:emit/k (yaml-kit:emit/k "value" #'capture))
                             (:write-yaml/k
                              (yaml-kit:write-yaml/k "value"
                                                     (make-string-output-stream)
                                                     #'capture))
                             (:edit-source/k
                              (yaml-kit:edit-source/k (format nil "a: 1~%")
                                                      '("a")
                                                      2
                                                      :k
                                                      #'capture))
                             (:%scanner-next-token/cps
                              (yaml-kit::%scanner-next-token/cps
                               (yaml-kit::make-scanner "value")
                               #'capture))
                             (:%parser-run/cps
                              (yaml-kit::%parser-run/cps
                               (lambda (event)
                                 (declare (ignore event)))
                               "value"
                               #'capture))
                             (:%composer-compose-all-events/cps
                              (yaml-kit::%composer-compose-all-events/cps
                               #'continuation-entry-source
                               #'capture))
                             (:%constructor-construct/cps
                              (yaml-kit::%constructor-construct/cps
                               (yaml-kit:make-scalar-node :value
                                                          "value"
                                                          :style
                                                          :plain)
                               #'capture))
                             (:%representer-represent/cps
                              (yaml-kit::%representer-represent/cps "value"
                                                                    #'capture))
                             (:%serializer-serialize/cps
                              (yaml-kit::%serializer-serialize/cps
                               (yaml-kit:make-scalar-node :value "value")
                               (lambda (event)
                                 (declare (ignore event)))
                               #'capture))
                             (:%emitter-emit-event-stream/cps
                              (yaml-kit::%emitter-emit-event-stream/cps
                               #'continuation-entry-source
                               (make-string-output-stream)
                               #'capture)))))
   (expect callback-count :to-equal 1)
   (expect
    (and (eq callback-return :callback-result)
         (or (not (eq entry :map-events/k)) (= handler-event-count 5)))
    :to-be-truthy)
   (expect (continuation-entry-value-matches-p entry callback-value)
           :to-be-truthy)))
