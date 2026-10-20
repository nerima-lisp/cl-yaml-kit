(in-package #:cl-yaml-kit/test)

(progn
  (defun make-yaml-emit-error-predicate (expected-message &optional expected-cause)
    (lambda (condition)
      (and (typep condition 'yaml-kit:yaml-emit-error)
           (string= (cl-parser-kit:diagnostic-message
                     (yaml-kit:yaml-kit-error-diagnostic condition))
                    expected-message)
           (or (null expected-cause)
               (equal (yaml-kit:yaml-emit-error-cause condition)
                      expected-cause)))))
  (cl-weave:it
   "reports unsupported representer values"
   (expect (lambda () (yaml-kit:emit :unsupported))
           :to-throw
           (make-yaml-emit-error-predicate "KEYWORD"))))

(cl-weave:it
 "reports unsupported representation nodes"
 (let ((node (eval '(defstruct (coverage-invalid-node
                               (:include yaml-kit::node)))))
       (handler (lambda (event) (declare (ignore event)))))
   (declare (ignore node))
   (expect
    (lambda ()
      (yaml-kit::serialize
       (funcall (symbol-function
                 (find-symbol "MAKE-COVERAGE-INVALID-NODE")))
       handler))
    :to-throw
    (make-yaml-emit-error-predicate "COVERAGE-INVALID-NODE"))))

(cl-weave:it
 "serializes a cyclic list with an alias"
 (let ((value (list nil)))
   (setf (car value) value)
   (expect value :to-emit-as #.(format nil "&id1 ~%- *id1~%"))))

(cl-weave:it-each
    ((positive-infinity #.(symbol-value 'sb-kernel::double-float-positive-infinity) ".inf")
     (negative-infinity #.(symbol-value 'sb-kernel::double-float-negative-infinity) "-.inf")
     (not-a-number #.(sb-kernel:make-double-float #x7ff80000 0) ".nan"))
  "emits special floating point values ~S"
  (name value expected)
  (declare (ignore name))
  (expect value :to-emit-as (format nil "~A~%" expected)))

(cl-weave:it-each
    ((null #.(symbol-value 'yaml-kit:+yaml-null+) #.(format nil "null~%"))
     (false #.(symbol-value 'yaml-kit:+yaml-false+) #.(format nil "false~%")))
  "emits YAML sentinel values ~S"
  (name value expected)
  (declare (ignore name))
  (expect value :to-emit-as expected))

(cl-weave:it
 "supports flow style and custom indentation"
 (expect (yaml-kit:emit '(1 2) :default-flow-style :flow :indent 4)
         :to-equal #.(format nil "[1, 2]~%"))
 (expect (yaml-kit:emit
          (yaml-kit:make-yaml-mapping (list (cons "a" (list 1 2))))
          :indent 4)
         :to-equal #.(format nil "a: ~%    - 1~%    - 2~%")))

(cl-weave:it
 "tracks emitter text state after ordinary text"
 (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
   (yaml-kit::%emit-text context "abc")
   (expect (list (yaml-kit::emitter-context-column context)
                 (yaml-kit::emitter-context-line-start context))
           :to-equal '(3 nil))))

(cl-weave:it
 "tracks emitter text state after a newline"
 (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
   (yaml-kit::%emit-text context (format nil "~%"))
   (expect (list (yaml-kit::emitter-context-column context)
                 (yaml-kit::emitter-context-line-start context))
           :to-equal '(0 t))))

(cl-weave:it
 "tracks emitter text state after an inline newline"
 (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
   (yaml-kit::%emit-text context "a")
   (yaml-kit::%emit-text context (format nil "~%b"))
   (expect (list (yaml-kit::emitter-context-column context)
                 (yaml-kit::emitter-context-line-start context))
           :to-equal '(1 nil))))

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
   (expect (yaml-kit:emit-events events)
           :to-equal #.(format nil "--- []~%"))))

(cl-weave:it
 "rejects an unknown emitter event"
 (expect (lambda () (yaml-kit::%emit-dispatch "unknown" nil nil))
         :to-throw
         (make-yaml-emit-error-predicate "YAML error" "unknown event")))

(cl-weave:it-each
    ((line-start nil #\Space (1 nil))
     (mid-line #\X #\Newline (0 t)))
  "updates emitter character state from both positions ~S"
  (name prefix character expected-state)
  (declare (ignore name))
  (let ((context (yaml-kit::make-emitter-context (make-string-output-stream) 2)))
    (when prefix (yaml-kit::%emit-char context prefix))
    (yaml-kit::%emit-char context character)
    (expect (list (yaml-kit::emitter-context-column context)
                  (yaml-kit::emitter-context-line-start context))
            :to-equal expected-state)))

(cl-weave:it-each
    ((null #\Null "\\0")
     (bell #.(code-char 7) "\\a")
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
(progn
  (cl-weave:it
   "returns only the primary value from emit/k"
   (let ((calls 0))
     (expect
      (multiple-value-list
       (yaml-kit:emit/k
        "value"
        (lambda (value)
          (declare (ignore value))
          (incf calls)
          (values :first :second))))
      :to-equal (list :first))
     (expect calls :to-equal 1)))
  (cl-weave:it
   "returns only the primary value from emit-events/k"
   (let ((calls 0)
         (events (list (yaml-kit:make-stream-start-event)
                       (yaml-kit:make-document-start-event)
                       (yaml-kit:make-scalar-event :value "value")
                       (yaml-kit:make-document-end-event)
                       (yaml-kit:make-stream-end-event))))
     (expect
      (multiple-value-list
       (yaml-kit:emit-events/k
        events
        (lambda (value)
          (declare (ignore value))
          (incf calls)
          (values :first :second))))
      :to-equal (list :first))
     (expect calls :to-equal 1)))
  (cl-weave:it
   "returns only the primary value from write-yaml/k"
   (let ((calls 0)
         (stream (make-string-output-stream)))
     (expect
      (multiple-value-list
       (yaml-kit:write-yaml/k
        "value" stream
        (lambda (value)
          (declare (ignore value))
          (incf calls)
          (values :first :second))))
      :to-equal (list :first))
     (expect calls :to-equal 1))))
