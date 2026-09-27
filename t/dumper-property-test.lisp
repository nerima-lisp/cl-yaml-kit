(in-package #:cl-yaml-kit/test)

(cl-weave:it
 "emits stream termination and explicit-key sequence prefixes"
 (expect (yaml-kit:emit-events
          (list (yaml-kit:make-stream-start-event)
                (yaml-kit:make-scalar-event :value "x")
                (yaml-kit:make-stream-end-event)))
         :to-equal #.(format nil "x~%"))
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map 0)))
   (yaml-kit::%emit-text context "x")
   (yaml-kit::%emit-sequence-start-frame
    state (yaml-kit:make-sequence-start-event :style :block)
    nil)
   (expect (not (null (search "? "
                              (get-output-stream-string
                               (yaml-kit::emitter-context-stream context)))))
           :to-equal t)))

(cl-weave:it
 "handles an explicit document after an explicit end"
 (let ((events (list (yaml-kit:make-stream-start-event)
                     (yaml-kit:make-document-start-event :explicit-p t)
                     (yaml-kit:make-scalar-event :value "one")
                     (yaml-kit:make-document-end-event :explicit-p t)
                     (yaml-kit:make-document-start-event :explicit-p t)
                     (yaml-kit:make-mapping-start-event)
                     (yaml-kit:make-mapping-end-event)
                     (yaml-kit:make-document-end-event)
                     (yaml-kit:make-stream-end-event))))
   (expect (yaml-kit:emit-events events) :to-be-truthy)))
