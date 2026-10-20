(in-package #:cl-yaml-kit/test)

(cl-weave:it-each
    ((plain "x" "tag:yaml.org,2002:str" "x")
     (reserved "true" "tag:yaml.org,2002:str" "'true'")
     (colon ": value" "tag:yaml.org,2002:str" "': value'"))
  "emits scalar event key values ~S"
  (name value tag expected)
  (declare (ignore name))
  (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
         (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
    (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 0)))
    (yaml-kit::%emit-scalar-event
     (yaml-kit:make-scalar-event :value value :style :plain :tag tag)
     state nil)
    (expect (get-output-stream-string
             (yaml-kit::emitter-context-stream context))
            :to-equal expected)))

(cl-weave:it-each
    ((plain nil "")
     (tagged "tag:x" "!<tag:x> ''"))
  "covers empty scalar event tag condition ~S"
  (name tag expected)
  (declare (ignore name))
  (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
         (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
    (yaml-kit::%emit-scalar-event
     (yaml-kit:make-scalar-event :value "" :style :plain :tag tag)
     state nil)
    (expect (get-output-stream-string
             (yaml-kit::emitter-context-stream context))
            :to-equal expected)))

(cl-weave:it-each
    ((empty nil ":")
     (tagged "tag:x" ": !<tag:x> x"))
  "covers scalar frame stack and tag predicates ~S"
  (name tag expected)
  (declare (ignore name))
  (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
         (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
    (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 1)))
    (yaml-kit::%emit-scalar-event
     (yaml-kit:make-scalar-event :value (if tag "x" "") :style :plain :tag tag)
     state nil)
    (expect (get-output-stream-string
             (yaml-kit::emitter-context-stream context))
            :to-equal expected)))

(cl-weave:it-each
    ((implicit nil #.(format nil "---~%"))
     (explicit t #.(format nil "%YAML 1.2~%---~%")))
  "emits %YAML 1.2 only for explicit non-initial document starts ~S"
  (name explicit-p expected)
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
    (expect (get-output-stream-string
             (yaml-kit::emitter-context-stream context))
            :to-equal expected)))

(cl-weave:it
 "keeps an empty stack empty and advances the active frame count for scalar events from zero to one"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (yaml-kit::%emit-scalar-event
    (yaml-kit:make-scalar-event :value "" :style :plain) state nil)
   (expect (yaml-kit::emitter-frame-state-stack state) :to-equal nil)
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map 0)))
   (expect (cdar (yaml-kit::emitter-frame-state-stack state))
           :to-equal 0)
   (yaml-kit::%emit-scalar-event
    (yaml-kit:make-scalar-event :value "" :style :plain) state nil)
   (expect (cdar (yaml-kit::emitter-frame-state-stack state))
           :to-equal 1)))

(cl-weave:it
 "keeps an empty stack empty and advances the active frame count for alias events from zero to one"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil))
        (event (yaml-kit:make-alias-event :anchor "a")))
   (yaml-kit::%emit-alias-event event state nil)
   (expect (yaml-kit::emitter-frame-state-stack state) :to-equal nil)
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map 0)))
   (expect (cdar (yaml-kit::emitter-frame-state-stack state))
           :to-equal 0)
   (yaml-kit::%emit-alias-event event state nil)
   (expect (cdar (yaml-kit::emitter-frame-state-stack state))
           :to-equal 1)))

(cl-weave:it-each
    ((block-map :map "")
     (flow-map :flow-map ", "))
  "emits a mapping separator for frame ~S"
  (name frame expected)
  (declare (ignore name))
  (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
         (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
    (setf (yaml-kit::emitter-frame-state-stack state) (list (cons frame 1)))
    (yaml-kit::%frame-separator state)
    (expect (get-output-stream-string
             (yaml-kit::emitter-context-stream context))
            :to-equal expected)))

(cl-weave:it
 "emits a flow sequence after an explicit key without a block-key marker"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map-after-explicit-key 0)))
   (yaml-kit::%emit-text context "x")
   (yaml-kit::%emit-sequence-start-frame
    state (yaml-kit:make-sequence-start-event :style :flow) nil)
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal (format nil "x~%"))))

(cl-weave:it
 "emits a block-key marker for a block sequence after an explicit key"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map-after-explicit-key 0)))
   (yaml-kit::%emit-text context "x")
   (yaml-kit::%emit-sequence-start-frame
    state (yaml-kit:make-sequence-start-event :style :block) nil)
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal (format nil "x~%? "))))

(cl-weave:it
 "separates a block mapping value after an existing key"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 1)))
   (yaml-kit::%emit-text context "x")
   (yaml-kit::%emit-mapping-start-frame
    state (yaml-kit:make-mapping-start-event :style :block) nil)
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal (format nil "x: ~%"))))