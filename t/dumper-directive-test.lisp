(in-package #:cl-yaml-kit/test)

(cl-weave:it
 "covers both line-start frame transitions"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map-after-explicit-key 1)))
   (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 1)))
   (yaml-kit::%frame-start-value
    state (yaml-kit:make-scalar-event :value "" :style :plain))
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map-after-explicit-key 1)))
   (yaml-kit::%emit-sequence-start-frame
    state (yaml-kit:make-sequence-start-event :style :flow) nil)
   (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 1)))
   (yaml-kit::%emit-mapping-start-frame
    state (yaml-kit:make-mapping-start-event :style :block) nil)
   (expect (stringp (get-output-stream-string
                     (yaml-kit::emitter-context-stream context)))
           :to-equal t)))

(cl-weave:it
 "covers independent frame predicate states"
 (flet ((state (stack)
          (yaml-kit::make-emitter-frame-state
           (yaml-kit::make-emitter-context (make-string-output-stream) 2)
           2 nil nil)))
   (let ((state (state (cons :map-after-explicit-key 1))))
     (setf (yaml-kit::emitter-frame-state-stack state)
           (list (cons :map-after-explicit-key 1)))
     (yaml-kit::%emit-sequence-start-frame
      state (yaml-kit:make-sequence-start-event :style :flow) nil))
   (let ((state (state (cons :map-after-explicit-key 1))))
     (setf (yaml-kit::emitter-frame-state-stack state)
           (list (cons :map-after-explicit-key 1)))
     (yaml-kit::%emit-text
      (yaml-kit::emitter-frame-state-context state) "x")
     (yaml-kit::%emit-sequence-start-frame
      state (yaml-kit:make-sequence-start-event :style :flow) nil))
   (dolist (tagged '(nil t))
     (dolist (written '(nil t))
       (let* ((context (yaml-kit::make-emitter-context
                        (make-string-output-stream) 2))
              (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
         (setf (yaml-kit::emitter-frame-state-stack state)
               (list (cons :map 1)))
         (when written (yaml-kit::%emit-text context "x"))
         (yaml-kit::%emit-mapping-start-frame
          state (yaml-kit:make-mapping-start-event
                 :style :block :tag (and tagged "tag:x")) nil))))
   (dolist (event (list (yaml-kit:make-scalar-event :value "" :style :plain)
                        (yaml-kit:make-scalar-event :value "" :style :plain
                                                    :tag "tag:x")))
     (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
            (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
       (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 1)))
       (yaml-kit::%frame-start-value state event)))
   (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
          (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
     (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 0)))
     (yaml-kit::%emit-mapping-start-frame
      state (yaml-kit:make-mapping-start-event :style :block) nil))
   (expect t :to-equal t)))

(cl-weave:it
 "covers plain safety and block scalar fallback"
 (expect (yaml-kit::%plain-safe-p "-" :tag "tag:yaml.org,2002:int")
         :to-equal nil)
 (expect (yaml-kit::%plain-safe-p "- value"
                                  :tag "tag:yaml.org,2002:int")
         :to-equal nil)
 (expect (yaml-kit::%scalar-style "" :literal nil)
         :to-equal :double-quoted)
 (expect (yaml-kit::%scalar-style (string (code-char 1)) :literal t)
         :to-equal :double-quoted))

(cl-weave:it
 "covers empty block scalar writer paths"
 (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
   (yaml-kit::%write-block-scalar "" context nil 2)
   (expect (get-output-stream-string (yaml-kit::emitter-context-stream context))
           :to-equal (format nil "|-~%"))))

(cl-weave:it-each
    ((plain "a")
     (kept "a~%")
     (more-kept "a~%~%")
     (indented "  value")
     (comment "# value")
     (blank-lines "a~%~%b"))
  "covers block scalar chomp and indentation paths ~S"
  (name value)
  (declare (ignore name))
  (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
    (yaml-kit::%write-block-scalar (if (member value '("a~%" "a~%~%" "a~%~%b"))
                                       (format nil value)
                                       value)
                                      context nil 2
                                      (not (string= value "a~%~%b")))
    (expect (plusp (length (get-output-stream-string
                            (yaml-kit::emitter-context-stream context))))
            :to-equal t)))

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
   (expect (search "---" (get-output-stream-string
                           (yaml-kit::emitter-context-stream context)))
           :to-be-truthy)))

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
   (expect (search "%YAML 1.2" (get-output-stream-string
                                 (yaml-kit::emitter-context-stream context)))
           :to-be-truthy)))