(in-package #:cl-yaml-kit/test)

(cl-weave:it-each
    ((tagged "tag:example.org,2026:str> \"value\"" :double-quoted "tag:example.org,2026:str" "value\"")
     (anchored "anchor :value" :plain nil "value"))
  "normalizes scalar event prefixes ~S"
  (name value style tag expected)
  (declare (ignore name))
  (let ((event (yaml-kit::%normalize-scalar-event
                (yaml-kit:make-scalar-event :value value :style :plain))))
    (expect (yaml-kit::scalar-event-style event) :to-equal style)
    (expect (yaml-kit::scalar-event-tag event) :to-equal tag)
    (expect (yaml-kit::scalar-event-value event) :to-equal expected)))

(cl-weave:it
 "covers direct scalar writer branches"
 (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
   (yaml-kit::%write-block-scalar (format nil "a~%~%b") context nil 2 nil)
   (yaml-kit::%write-single-quoted (format nil "a'b~%~%c") context 2)
   (yaml-kit::%write-plain (format nil "a~%b") context)
   (yaml-kit::%write-double-quoted
    (string (code-char #xfeff)) context)
   (expect (search "|" (get-output-stream-string
           (yaml-kit::emitter-context-stream context)))
           :to-be-truthy)))

(cl-weave:it-each
    ((bang "!")
     (local "!local")
     (tag "tag:example.org,2026:type")
     (plain "custom"))
  "emits direct tag prefixes ~S"
  (name tag)
  (declare (ignore name))
  (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
    (yaml-kit::%emit-prefix
     (yaml-kit:make-scalar-event :tag tag :value "x") context)
    (expect (not (null (search "!"
                               (get-output-stream-string
                                (yaml-kit::emitter-context-stream context)))))
            :to-equal t)))

(cl-weave:it
 "covers blank-line and character state helpers"
 (expect (yaml-kit::%blank-line-p "" 0) :to-be-truthy)
 (expect (yaml-kit::%blank-line-p "  " 0) :to-be-truthy)
 (expect (yaml-kit::%blank-line-p #.(format nil "x~%") 0) :to-equal nil)
 (expect (yaml-kit::%blank-line-p #.(format nil " ~%") 0) :to-be-truthy)
 (expect (yaml-kit::%blank-line-p "x" 0) :to-be-truthy)
 (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
   (yaml-kit::%emit-char context #\A)
   (expect (yaml-kit::emitter-context-column context) :to-equal 1)
   (yaml-kit::%emit-char context #\Newline)
   (expect (yaml-kit::emitter-context-line-start context) :to-equal t)))

(cl-weave:it-each
    ((document-start "---")
     (document-end "...")
     (flow-colon "a: b")
     (plain-colon "a:b")
     (hash-start "#x")
     (hash-middle "a #x")
     (dash "- x")
     (dash-word "-x")
     (space-break "a \nb"))
  "covers scalar analysis boundaries ~S"
  (name value)
  (declare (ignore name))
  (expect (yaml-kit::%scalar-analysis value) :to-be-truthy))

(cl-weave:it
 "covers scalar safety and block-style fallback branches"
 (expect (not (null (yaml-kit::%plain-safe-p
                     "-value" :tag "tag:yaml.org,2002:int")))
         :to-equal t)
 (expect (yaml-kit::%scalar-style (format nil "a~%b") :plain nil
                                  "tag:example.org,2026:str")
         :to-equal :double-quoted)
 (expect (yaml-kit::%scalar-style "a" :literal t nil)
         :to-equal :double-quoted))

(cl-weave:it
 "covers frame separator and explicit key branches"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map 1))
         (yaml-kit::emitter-frame-state-last-key-style state) :double-quoted)
   (yaml-kit::%frame-start-value
    state (yaml-kit:make-sequence-start-event :style :block))
   (expect (not (null (search ":"
                              (get-output-stream-string
                               (yaml-kit::emitter-context-stream context)))))
           :to-equal t)))

(cl-weave:it
 "covers explicit mapping sequence frame separators"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :map 2))
         (yaml-kit::emitter-frame-state-last-key-style state) :double-quoted)
   (yaml-kit::%emit-text context "key")
   (yaml-kit::%frame-start-value
    state (yaml-kit:make-sequence-start-event :style :block))
   (expect (get-output-stream-string (yaml-kit::emitter-context-stream context))
           :to-equal "key:")))

(cl-weave:it
 "covers flow frame separators after a mapping key"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state)
         (list (cons :flow-map 2)))
   (yaml-kit::%frame-start-value
    state (yaml-kit:make-sequence-start-event :style :flow))
   (expect (get-output-stream-string (yaml-kit::emitter-context-stream context))
           :to-equal ", ")))

(cl-weave:it
 "covers tagged scalar and mapping frame conditions"
 (let* ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2))
        (state (yaml-kit::make-emitter-frame-state context 2 nil nil)))
   (setf (yaml-kit::emitter-frame-state-stack state) (list (cons :map 1)))
   (yaml-kit::%frame-start-value
    state (yaml-kit:make-scalar-event :value "x" :style :plain :tag "tag:x"))
   (yaml-kit::%emit-text context "x")
   (yaml-kit::%emit-mapping-start-frame
    state (yaml-kit:make-mapping-start-event :style :block :tag "tag:x") nil)
   (expect (plusp (length (get-output-stream-string
                           (yaml-kit::emitter-context-stream context))))
           :to-equal t)))