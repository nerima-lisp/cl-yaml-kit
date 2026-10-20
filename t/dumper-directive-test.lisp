(in-package #:cl-yaml-kit/test)

(cl-weave:it
 "formats an empty scalar key, an explicit-key sequence, and a block mapping"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 1)))
   (yaml-kit::%frame-start-value
    state (yaml-kit:make-scalar-event :value "" :style :plain))
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal ":")
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map-after-explicit-key 1)))
   (yaml-kit::%emit-sequence-start-frame
    state (yaml-kit:make-sequence-start-event :style :flow) nil)
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal (format nil "~%: "))
   (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 1)))
   (yaml-kit::%emit-mapping-start-frame
    state (yaml-kit:make-mapping-start-event :style :block) nil)
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal (format nil ": ~%"))))

(cl-weave:it-each
    ((explicit-key-empty-flow-sequence :explicit-key :flow-sequence nil nil ": ")
     (explicit-key-text-flow-sequence :explicit-key :flow-sequence "x" nil #.(format nil "x~%: "))
     (empty-mapping-value :mapping :mapping nil nil #.(format nil ": ~%"))
     (mapping-value :mapping :mapping "x" nil #.(format nil "x: ~%"))
     (tagged-empty-mapping-value :mapping :mapping nil "tag:x" #.(format nil ": !<tag:x> ~%"))
     (tagged-mapping-value :mapping :mapping "x" "tag:x" #.(format nil "x: !<tag:x> ~%"))
     (plain-empty-scalar-value :mapping :empty-scalar nil nil ":")
     (tagged-empty-scalar-value :mapping :empty-scalar nil "tag:x" ": ")
     (empty-block-mapping :empty-mapping :empty-mapping nil nil "? "))
  "emits expected output for mapping, scalar, and sequence frame cases ~S"
  (name frame-kind operation text tag expected)
  (declare (ignore name))
  (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
         (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
    (setf (yaml-kit::emitter-frame-state-stack state)
          (ecase frame-kind
            (:explicit-key (list (cons :map-after-explicit-key 1)))
            (:mapping (list (cons :map 1)))
            (:empty-mapping (list (cons :map 0)))))
    (when text (yaml-kit::%emit-text context text))
    (ecase operation
      (:flow-sequence
       (yaml-kit::%emit-sequence-start-frame
        state (yaml-kit:make-sequence-start-event :style :flow) nil))
      (:mapping
       (yaml-kit::%emit-mapping-start-frame
        state (yaml-kit:make-mapping-start-event :style :block :tag tag) nil))
      (:empty-scalar
       (yaml-kit::%frame-start-value
        state (yaml-kit:make-scalar-event :value "" :style :plain :tag tag)))
      (:empty-mapping
       (yaml-kit::%emit-mapping-start-frame
        state (yaml-kit:make-mapping-start-event :style :block) nil)))
    (expect (get-output-stream-string
             (yaml-kit::emitter-context-stream context))
            :to-equal expected)))

(cl-weave:it-each
    ((leading-dash "-" nil)
     (dash-space "- value" nil))
  "rejects unsafe integer plain scalars ~S"
  (name value expected)
  (declare (ignore name))
  (expect (yaml-kit::%plain-safe-p
           value :tag "tag:yaml.org,2002:int")
          :to-equal expected))

(cl-weave:it-each
    ((empty-literal "" :literal nil :double-quoted)
     (control-literal #.(string (code-char 1))
                      :literal t :double-quoted))
  "selects quoted fallbacks for literal scalars ~S"
  (name value style flow expected)
  (declare (ignore name))
  (expect (yaml-kit::%scalar-style value style flow)
          :to-equal expected))

(cl-weave:it
 "covers empty block scalar writer paths"
 (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
   (yaml-kit::%write-block-scalar "" context nil 2)
   (expect (get-output-stream-string (yaml-kit::emitter-context-stream context))
           :to-equal (format nil "|-~%"))))

(cl-weave:it-each
    ((plain "a" #.(format nil "|-~%  a~%") t)
     (kept #.(format nil "a~%") #.(format nil "|~%  a~%") t)
     (more-kept #.(format nil "a~%~%") #.(format nil "|+~%  a~%~%") t)
     (indented "  value" #.(format nil "|2-~%    value~%") t)
     (comment "# value" #.(format nil "|2-~%  # value~%") t)
     (unindented-blank-lines #.(format nil "a~%~%b")
                             #.(format nil "|-~%  a~%~%  b~%") nil))
  "writes block scalar chomping, indentation, and blank-line policy cases ~S"
  (name value expected preserve-blank-indentation)
  (declare (ignore name))
  (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
    (yaml-kit::%write-block-scalar
     value context nil 2 preserve-blank-indentation)
    (expect (get-output-stream-string
             (yaml-kit::emitter-context-stream context))
            :to-equal expected)))

(cl-weave:it
 "covers the non-initial document separator path"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-first-document state) nil)
   (yaml-kit::%emit-text context "x")
   (yaml-kit::%emit-document-start-event
    (yaml-kit:make-document-start-event :explicit-p t)
    state
    (list (yaml-kit:make-mapping-start-event)
          (yaml-kit:make-mapping-end-event)))
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal (format nil "x~%---~%"))))

(cl-weave:it
 "covers a versioned non-initial document"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-first-document state) nil
         (yaml-kit::emitter-frame-state-previous-document-explicit-end state) t)
   (yaml-kit::%emit-document-start-event
    (yaml-kit:make-document-start-event :explicit-p t :version "1.2")
    state
    (list (yaml-kit:make-mapping-start-event)
          (yaml-kit:make-mapping-end-event)))
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal (format nil "%YAML 1.2~%---~%"))))
