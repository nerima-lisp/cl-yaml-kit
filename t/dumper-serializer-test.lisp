(in-package #:cl-yaml-kit/test)

(cl-weave:it
 "reports unsupported representer values"
 (expect (handler-case (progn (yaml-kit:emit :unsupported) nil)
           (yaml-kit:yaml-emit-error () t))
         :to-equal t))

(cl-weave:it
 "reports unsupported representation nodes"
 (let ((node (eval '(defstruct (coverage-invalid-node
                                (:include yaml-kit::node)))))
       (handler (lambda (event) (declare (ignore event)))))
   (declare (ignore node))
   (expect (handler-case
               (progn (yaml-kit::serialize
                       (funcall (symbol-function
                                 (find-symbol "MAKE-COVERAGE-INVALID-NODE")))
                       handler)
                      nil)
             (yaml-kit:yaml-emit-error () t))
           :to-equal t)))

(cl-weave:it
 "serializes a cyclic list with an alias"
 (let ((value (list nil)))
   (setf (car value) value)
   (expect (yaml-kit:emit value) :to-equal #.(format nil "&id1 ~%- *id1~%"))))

(cl-weave:it-each
    ((positive-infinity #.(symbol-value 'sb-kernel::double-float-positive-infinity) ".inf")
     (negative-infinity #.(symbol-value 'sb-kernel::double-float-negative-infinity) "-.inf")
     (not-a-number #.(sb-kernel:make-double-float #x7ff80000 0) ".nan"))
  "emits special floating point values ~S"
  (name value expected)
  (declare (ignore name))
  (expect (yaml-kit:emit value) :to-equal (format nil "~A~%" expected)))

(cl-weave:it
 "emits YAML sentinel values"
 (expect (yaml-kit:emit yaml-kit:+yaml-null+) :to-equal #.(format nil "null~%"))
 (expect (yaml-kit:emit yaml-kit:+yaml-false+) :to-equal #.(format nil "false~%")))

(cl-weave:it
 "supports flow style and custom indentation"
 (expect (yaml-kit:emit '(1 2) :default-flow-style :flow :indent 4)
         :to-equal #.(format nil "[1, 2]~%"))
 (expect (yaml-kit:emit
          (yaml-kit:make-yaml-mapping (list (cons "a" (list 1 2))))
          :indent 4)
         :to-equal #.(format nil "a: ~%    - 1~%    - 2~%")))

(cl-weave:it
 "tracks emitter text state"
 (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
    (yaml-kit::%emit-text context "abc")
    (expect (yaml-kit::emitter-context-column context) :to-equal 3)
    (expect (yaml-kit::emitter-context-line-start context) :to-equal nil)
    (yaml-kit::%emit-text context (format nil "~%"))
    (expect (yaml-kit::emitter-context-column context) :to-equal 0)
    (expect (yaml-kit::emitter-context-line-start context) :to-equal t)
    (yaml-kit::%emit-text context "a")
    (yaml-kit::%emit-text context (format nil "~%b"))
    (expect (yaml-kit::emitter-context-column context) :to-equal 1)
    (expect (yaml-kit::emitter-context-line-start context) :to-equal nil)))

(cl-weave:it
 "emits event directives and explicit document ends"
 (let ((events (list (yaml-kit:make-stream-start-event)
                     (yaml-kit:make-document-start-event
                      :explicit-p t :version "1.2"
                      :tag-directives '(("!e!" . "tag:example.com,2026:")))
                     (yaml-kit:make-scalar-event :value "ok")
                     (yaml-kit:make-document-end-event :explicit-p t)
                     (yaml-kit:make-stream-end-event))))
   (expect (yaml-kit:emit-events events)
           :to-equal #.(format nil "%YAML 1.2~%%TAG !e! tag:example.com,2026:~%--- ok~%...~%"))))

(cl-weave:it
 "emits an empty flow collection from events"
 (let ((events (list (yaml-kit:make-stream-start-event)
                     (yaml-kit:make-document-start-event :explicit-p t)
                     (yaml-kit:make-sequence-start-event :style :flow)
                     (yaml-kit:make-sequence-end-event)
                     (yaml-kit:make-document-end-event)
                     (yaml-kit:make-stream-end-event))))
   (expect (yaml-kit:emit-events events) :to-equal #.(format nil "--- []~%"))))

(cl-weave:it
 "rejects an unknown emitter event"
 (expect (handler-case (progn (yaml-kit::%emit-dispatch "unknown" nil nil) nil)
           (yaml-kit:yaml-emit-error () t))
         :to-equal t))

(cl-weave:it
 "emits indentation from both context positions"
 (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
   (yaml-kit::%emit-indent context 1)
   (expect (yaml-kit::emitter-context-line-start context) :to-equal nil)
   (yaml-kit::%emit-text context "x")
   (yaml-kit::%emit-indent context 2)
   (expect (get-output-stream-string (yaml-kit::emitter-context-stream context))
           :to-equal #.(format nil "  x~%    "))))

(cl-weave:it-each
    ((null #\Null "\\0")
     (bell #\Bell "\\a")
     (backspace #\Backspace "\\b")
     (tab #\Tab "\\t")
     (newline #\Newline "\\n")
     (vertical-tab #\Vt "\\v")
     (page #\Page "\\f")
     (return #\Return "\\r")
     (escape #\Escape "\\e")
     (quote #\" "\\\"")
     (backslash #\\ "\\\\")
     (nel #.(code-char #x85) "\\N")
     (nbsp #.(code-char #xa0) "\\_")
     (line-separator #.(code-char #x2028) "\\L")
     (paragraph-separator #.(code-char #x2029) "\\P"))
  "escapes double-quoted scalar characters ~S"
  (name character escape)
  (declare (ignore name))
  (expect (yaml-kit:emit-events
           (list (yaml-kit:make-stream-start-event)
                 (yaml-kit:make-document-start-event)
                 (yaml-kit:make-scalar-event :value (string character)
                                             :style :double-quoted)
                 (yaml-kit:make-document-end-event)
                 (yaml-kit:make-stream-end-event)))
          :to-equal (format nil "\"~A\"~%" escape)))

(cl-weave:it
 "expands the representer builder macro"
 (let ((expansion (macroexpand-1
                   '(yaml-kit::define-representer-builder test-builder
                      yaml-kit::make-scalar-node yaml-kit::scalar-node-value
                      value))))
   (expect (first expansion) :to-equal 'defun)
   (expect (second expansion) :to-equal 'test-builder)))

(cl-weave:it
 "expands the representer dispatch macro"
 (let ((expansion (macroexpand-1
                   '(yaml-kit::define-representer-dispatch test-dispatch
                      ((string yaml-kit::%represent-scalar))))) )
   (expect (first expansion) :to-equal 'defun)
   (expect (second expansion) :to-equal 'test-dispatch)))