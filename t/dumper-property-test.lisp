(in-package #:cl-yaml-kit/test)

(cl-weave:it
 "terminates an event stream after a scalar"
 (expect (yaml-kit:emit-events
          (list (yaml-kit:make-stream-start-event)
                (yaml-kit:make-scalar-event :value "x")
                (yaml-kit:make-stream-end-event)))
         :to-equal #.(format nil "x~%")))

(cl-weave:it
 "prefixes a block sequence after an explicit key"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map 0)))
   (yaml-kit::%emit-text context "x")
   (yaml-kit::%emit-sequence-start-frame
    state (yaml-kit:make-sequence-start-event :style :block)
    nil)
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal #.(format nil "x~%? "))))

(cl-weave:it
 "emits an explicit document after an explicit end"
 (let ((events (list (yaml-kit:make-stream-start-event)
                     (yaml-kit:make-document-start-event :explicit-p t)
                     (yaml-kit:make-scalar-event :value "one")
                     (yaml-kit:make-document-end-event :explicit-p t)
                     (yaml-kit:make-document-start-event :explicit-p t)
                     (yaml-kit:make-mapping-start-event)
                     (yaml-kit:make-mapping-end-event)
                     (yaml-kit:make-document-end-event)
                     (yaml-kit:make-stream-end-event))))
   (expect (yaml-kit:emit-events events)
           :to-equal #.(format nil "--- one~%...~%%YAML 1.2~%---~%{}~%"))))

(cl-weave:it-property
 "emitted scalar event streams preserve generated YAML-sensitive strings"
 ((value (cl-weave:gen-string :min-length 1 :max-length 24
                              :alphabet "abcXYZ012 :#[]{}&*!|\"%@`- ")))
 (let ((yaml (yaml-kit:emit-events
              (list (yaml-kit:make-stream-start-event)
                    (yaml-kit:make-scalar-event :value value)
                    (yaml-kit:make-stream-end-event)))))
   (expect yaml :to-parse-as value)
   (expect value :to-round-trip t)))

(cl-weave:it-fuzz
 "emits generated plain strings exactly"
 ((value (cl-weave:gen-string :min-length 1 :max-length 32
                              :alphabet "abcXYZ")))
 (:trials 200 :timeout-per-trial 5)
 (expect (yaml-kit:emit value)
         :to-equal (format nil "~A~%" value)))
