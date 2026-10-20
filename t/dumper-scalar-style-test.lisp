(in-package #:cl-yaml-kit/test)

(cl-weave:it
 "writes scalar styles directly"
 (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
   (yaml-kit::%write-block-scalar (format nil "a~%~%b") context nil 2 nil)
   (yaml-kit::%write-single-quoted (format nil "a'b~%~%c") context 2)
   (yaml-kit::%write-plain (format nil "a~%b") context)
   (yaml-kit::%write-double-quoted
    (string (code-char #xfeff)) context)
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal (format nil "|-~%  a~%~%  b~%'a''b~%~%~%  c'a~%~%b\"\\uFEFF\""))))

(cl-weave:it-each
    ((verbatim-tag nil "!" "! ")
     (local-tag nil "!local" "!local ")
     (global-tag nil "tag:example.org,2026:type"
                 "!<tag:example.org,2026:type> ")
     (custom-tag nil "custom" "!custom ")
     (implicit-string nil "tag:yaml.org,2002:str" "")
     (anchored "node" nil "&node "))
  "emits tag and anchor prefixes ~S"
  (name anchor tag expected)
  (declare (ignore name))
  (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
    (yaml-kit::%emit-prefix
     (yaml-kit:make-scalar-event :anchor anchor :tag tag :value "x") context)
    (expect (get-output-stream-string
             (yaml-kit::emitter-context-stream context))
            :to-equal expected)))

(cl-weave:it
 "tracks emitter character state"
 (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
   (yaml-kit::%emit-char context #\A)
   (expect (yaml-kit::emitter-context-column context) :to-equal 1)
   (yaml-kit::%emit-char context #\Newline)
   (expect (yaml-kit::emitter-context-column context) :to-equal 0)
   (expect (yaml-kit::emitter-context-line-start context) :to-equal t)))

(cl-weave:it-each
    ((document-start "---"
                     (:multiline nil :flow-plain nil :block-plain nil
                       :single-quoted t :block t))
     (document-end "..."
                   (:multiline nil :flow-plain nil :block-plain nil
                     :single-quoted t :block t))
     (flow-colon "a: b"
                 (:multiline nil :flow-plain nil :block-plain nil
                   :single-quoted t :block t))
     (plain-colon "a:b"
                  (:multiline nil :flow-plain nil :block-plain t
                    :single-quoted t :block t))
     (flow-question "a?b"
                    (:multiline nil :flow-plain nil :block-plain t
                      :single-quoted t :block t))
     (hash-start "#x"
                 (:multiline nil :flow-plain nil :block-plain nil
                   :single-quoted t :block t))
     (hash-middle "a #x"
                  (:multiline nil :flow-plain nil :block-plain nil
                    :single-quoted t :block t))
     (dash "- x"
           (:multiline nil :flow-plain nil :block-plain nil
             :single-quoted t :block t))
     (dash-word "-x"
                (:multiline nil :flow-plain nil :block-plain nil
                  :single-quoted t :block t))
     (space-break #.(format nil "a ~%b")
                  (:multiline t :flow-plain nil :block-plain nil
                    :single-quoted nil :block t)))
  "analyzes scalar boundaries ~S"
  (name value expected)
  (declare (ignore name))
  (expect (yaml-kit::%scalar-analysis value) :to-equal expected))

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
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal ":")))

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
   (expect (get-output-stream-string
            (yaml-kit::emitter-context-stream context))
           :to-equal (format nil ": x: !<tag:x> ~%"))))