(in-package #:cl-yaml-kit/test)

(it "attaches composer resource-limit errors to the triggering event span"
  (let* ((span (cl-parser-kit:make-span :source nil
                                        :start 7 :end 12
                                        :start-line 2 :start-column 1
                                        :end-line 2 :end-column 6))
         (event-source
           (yaml-kit::event-list-source
            (list (yaml-kit:make-stream-start-event)
                  (yaml-kit:make-document-start-event)
                  (yaml-kit:make-scalar-event :start-span span
                                              :end-span span
                                              :value "value")
                  (yaml-kit:make-document-end-event)
                  (yaml-kit:make-stream-end-event)))))
    (expect (lambda ()
              (yaml-kit:compose-all-events event-source :max-nodes 0))
            :to-throw
            (lambda (condition)
              (and (typep condition 'yaml-kit:yaml-resource-limit-error)
                   (let ((diagnostic (yaml-kit:yaml-kit-error-diagnostic condition)))
                     (and (string= "YAML resource limit \"nodes\" exceeded: limit 0, actual 1"
                                   (cl-parser-kit:diagnostic-message diagnostic))
                          (equal span (cl-parser-kit:diagnostic-span diagnostic)))))))))

(it "returns only primary composer continuation values"
  (let ((event-source
          (yaml-kit::event-list-source (yaml-kit:parse-events "value"))))
    (expect (multiple-value-list
             (yaml-kit:compose-events/k
              event-source
              (lambda (node)
                (declare (ignore node))
                (values :primary :secondary))))
            :to-equal
            '(:primary))
    (expect (multiple-value-list
             (yaml-kit:compose-all-events/k
              event-source
              (lambda (nodes)
                (declare (ignore nodes))
                (values :primary :secondary))))
            :to-equal
            '(:primary))))

(it "ignores an event source return value without a document"
  (expect (yaml-kit:compose-all-events
           (lambda (handler)
             (declare (ignore handler))
             :unrelated)
           :first-only t)
          :to-equal nil))

(it "keeps compose-events first-only when options include first-only"
  (let* ((events (yaml-kit:parse-events
                  #.(format nil "first~%---~%second")))
         (event-count 0)
         (event-source
           (lambda (handler)
             (dolist (event events)
               (incf event-count)
               (funcall handler event)))))
    (expect (yaml-kit:scalar-node-value
             (yaml-kit:compose-events event-source :first-only nil))
            :to-equal
            "first")
    (expect event-count :to-equal 4)))
