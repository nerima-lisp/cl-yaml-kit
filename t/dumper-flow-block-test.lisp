(in-package #:cl-yaml-kit/test)

(cl-weave:it-each
    ((plain "x")
     (reserved "true")
     (colon ": value"))
  "covers scalar event key and tag conditions ~S"
  (name value)
  (declare (ignore name))
  (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
         (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
    (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 0)))
    (yaml-kit::%emit-scalar-event
     (yaml-kit:make-scalar-event :value value :style :plain)
     state nil)
    (expect (plusp (length (get-output-stream-string
                            (yaml-kit::emitter-context-stream context))))
            :to-equal t)))

(cl-weave:it-each
    ((plain nil)
     (tagged "tag:x"))
  "covers empty scalar event tag condition ~S"
  (name tag)
  (declare (ignore name))
  (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
         (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
    (yaml-kit::%emit-scalar-event
     (yaml-kit:make-scalar-event :value "" :style :plain :tag tag)
     state nil)
    (expect (stringp (get-output-stream-string
                      (yaml-kit::emitter-context-stream context)))
            :to-equal t)))

(cl-weave:it-each
    ((empty nil)
     (tagged "tag:x"))
  "covers scalar frame stack and tag predicates ~S"
  (name tag)
  (declare (ignore name))
  (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
         (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
    (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 1)))
    (yaml-kit::%emit-scalar-event
     (yaml-kit:make-scalar-event :value (if tag "x" "") :style :plain :tag tag)
     state nil)
    (expect (stringp (get-output-stream-string
                      (yaml-kit::emitter-context-stream context)))
            :to-equal t)))

(cl-weave:it-each
    ((implicit nil)
     (explicit t))
  "covers document explicit predicate ~S"
  (name explicit-p)
  (declare (ignore name))
  (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
         (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
    (setf (yaml-kit::emitter-frame-state-first-document state) nil
          (yaml-kit::emitter-frame-state-previous-document-explicit-end state) t)
    (yaml-kit::%emit-document-start-event
     (yaml-kit:make-document-start-event :explicit-p explicit-p)
     state
     (list (yaml-kit:make-mapping-start-event)
           (yaml-kit:make-mapping-end-event)))
    (expect (stringp (get-output-stream-string
                      (yaml-kit::emitter-context-stream context)))
            :to-equal t)))

(cl-weave:it
 "covers scalar event stack presence directly"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (yaml-kit::%emit-scalar-event
    (yaml-kit:make-scalar-event :value "") state nil)
   (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 0)))
   (yaml-kit::%emit-scalar-event
    (yaml-kit:make-scalar-event :value "x") state nil)
   (expect (stringp (get-output-stream-string
                     (yaml-kit::emitter-context-stream context)))
           :to-equal t)))

(cl-weave:it
 "covers alias event stack presence"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil))
        (event (yaml-kit:make-alias-event :anchor "a")))
   (yaml-kit::%emit-alias-event event state nil)
   (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 0)))
   (yaml-kit::%emit-alias-event event state nil)
   (expect (search "*a" (get-output-stream-string
                          (yaml-kit::emitter-context-stream context)))
           :to-be-truthy)))

(cl-weave:it
 "covers flow and block frame separators"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 1)))
   (yaml-kit::%frame-separator state)
   (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :flow-map 1)))
   (yaml-kit::%frame-separator state)
   (expect (get-output-stream-string (yaml-kit::emitter-context-stream context))
           :to-equal ", ")))

(cl-weave:it
 "covers a non-line-start explicit-key sequence"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map-after-explicit-key 0)))
   (yaml-kit::%emit-text context "x")
   (yaml-kit::%emit-sequence-start-frame
    state (yaml-kit:make-sequence-start-event :style :flow) nil)
   (expect (plusp (length (get-output-stream-string
                           (yaml-kit::emitter-context-stream context))))
           :to-equal t)))

(cl-weave:it
 "covers non-line-start block sequence and mapping frames"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map-after-explicit-key 0)))
   (yaml-kit::%emit-text context "x")
   (yaml-kit::%emit-sequence-start-frame
    state (yaml-kit:make-sequence-start-event :style :block) nil)
   (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 1)))
   (yaml-kit::%emit-text context "x")
   (yaml-kit::%emit-mapping-start-frame
    state (yaml-kit:make-mapping-start-event :style :block) nil)
   (expect (plusp (length (get-output-stream-string
                           (yaml-kit::emitter-context-stream context))))
           :to-equal t)))