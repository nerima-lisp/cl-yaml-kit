;;;; t/dumper-test.lisp
(in-package #:cl-yaml-kit/test)

(defun dump-to-string-with-newline (value)
  (with-output-to-string (stream)
    (yaml-kit:write-yaml value stream)))

(describe
  "dumper"
  (cl-weave:it-each
    ((nil #.(format nil "[]~%"))
     (t #.(format nil "true~%"))
     (42 #.(format nil "42~%"))
     ("hello" #.(format nil "hello~%"))
     (("one" "two") #.(format nil "- one~%- two~%")))
    "dumps ~S as ~S"
    (value expected)
    (expect (dump-to-string-with-newline value) :to-equal expected))

  (cl-weave:it-each
    (("" #.(format nil "''~%"))
     ("- item" #.(format nil "'- item'~%"))
     ("? item" #.(format nil "'? item'~%"))
     (": item" #.(format nil "': item'~%"))
     ("-alpha" #.(format nil "'-alpha'~%"))
     ("?alpha" #.(format nil "'?alpha'~%"))
     (":alpha" #.(format nil "':alpha'~%"))
     ("# comment" #.(format nil "'# comment'~%"))
     ("key: value" #.(format nil "'key: value'~%"))
     ("value # comment" #.(format nil "'value # comment'~%"))
     ("a:b" #.(format nil "a:b~%"))
     ("plain scalar" #.(format nil "plain scalar~%")))
    "quotes plain-scalar boundary value ~S"
    (value expected)
    (expect (dump-to-string-with-newline value) :to-equal expected)))

(cl-weave:it-each
 (("true" "'true'")
  ("1.5" "'1.5'")
  ("0x1F" "'0x1F'")
  ("~" "'~'")
  ("" "''")
  (": " "': '")
  ("- a" "'- a'")
  ("#x" "'#x'")
  (" lead" "' lead'")
  ("trail " "'trail '")
  (#.(format nil "a~%b") "\"a\\nb\"")
  (#.(format nil " folded~%") "\" folded\\n\"")
     (#.(string (code-char 1)) "\"\\x01\""))
  "emits scalar boundary ~S as ~S"
  (value expected)
  (expect (dump-to-string-with-newline value)
          :to-equal (format nil "~A~%" expected)))

(cl-weave:it-each
    ((empty-string "")
     (leading-space " lead")
     (trailing-space "trail ")
     (both-edge-spaces " edge ")
     (space-only " "))
  "reads back emitted scalar boundary value ~S"
  (name value)
  (declare (ignore name))
  (expect (yaml-kit:parse (yaml-kit:emit value)) :to-equal value))

(defun emit-events-to-string (events)
  (yaml-kit:emit-events events))

(defun regression-events (value sequence-p)
  (append (list (yaml-kit:make-stream-start-event)
                (yaml-kit:make-document-start-event))
          (when sequence-p (list (yaml-kit:make-sequence-start-event)))
          (list (yaml-kit:make-scalar-event :value value :style :literal))
          (when sequence-p (list (yaml-kit:make-sequence-end-event)))
          (list (yaml-kit:make-document-end-event)
                (yaml-kit:make-stream-end-event))))

(cl-weave:it-each
    ((block-scalar-indentation
       #.(format nil "detected~%")
       t
       #.(format nil "- |~%  detected~%"))
     (literal-preserves-trailing-space
       #.(format nil "ab~%~% ~%")
       nil
       #.(format nil "|~%  ab~%  ~%   ~%")))
  "emits dumper event regression ~S"
  (name value sequence-p expected)
  (declare (ignore name))
  (expect (emit-events-to-string
           (regression-events value sequence-p))
          :to-equal expected))

(cl-weave:it
 "emits an empty stream event input"
 (expect (emit-events-to-string
          (list (yaml-kit:make-stream-start-event)
                (yaml-kit:make-stream-end-event)))
         :to-equal ""))

(cl-weave:it
 "emits an empty document event input"
 (expect (emit-events-to-string
          (list (yaml-kit:make-stream-start-event)
                (yaml-kit:make-document-start-event)
                (yaml-kit:make-document-end-event)
                (yaml-kit:make-stream-end-event)))
         :to-equal #.(format nil "---~%")))

(cl-weave:it-each
    ((character #\A #.(format nil "A~%"))
     (float 1.5d0 #.(format nil "1.5~%"))
     (vector #(1 2) #.(format nil "- 1~%- 2~%")))
  "dumps representer dispatch cases ~S"
  (name value expected)
  (declare (ignore name))
  (expect (yaml-kit:emit value) :to-equal expected))

(cl-weave:it
 "dumps a hash table and preserves an explicit document marker"
 (let ((table (make-hash-table :test #'equal)))
   (setf (gethash "answer" table) 42)
   (expect (yaml-kit:emit table :explicit-document-start t)
           :to-equal #.(format nil "---~%answer: 42~%"))))

(cl-weave:it
 "dumps an empty mapping"
 (expect (yaml-kit:emit (yaml-kit:make-yaml-mapping nil))
         :to-equal #.(format nil "{}~%")))

(cl-weave:it
 "supports event lists and write-yaml"
 (let ((events (list (yaml-kit:make-stream-start-event)
                     (yaml-kit:make-document-start-event)
                     (yaml-kit:make-scalar-event :value "ok")
                     (yaml-kit:make-document-end-event)
                     (yaml-kit:make-stream-end-event))))
   (expect (yaml-kit:emit-events events)
           :to-equal #.(format nil "ok~%"))
   (with-output-to-string (stream)
     (expect (yaml-kit:write-yaml 7 stream) :to-equal 7)
     (expect (get-output-stream-string stream) :to-equal #.(format nil "7~%")))))

(cl-weave:it-each
    ((plain-analysis "plain")
     (flow-analysis "a,b")
     (line-break-analysis #.(format nil "a~%b"))
     (control-analysis #.(string (code-char 1)))
     (unicode-analysis #.(string (code-char #x2028))))
  "covers scalar analysis and escaping boundaries ~S"
  (name value)
  (declare (ignore name))
  (expect (not (null (yaml-kit::%scalar-analysis value))) :to-equal t)
  (expect (not (null (member (yaml-kit::%scalar-style value :plain nil)
                             '(:plain :single-quoted :double-quoted))))
          :to-equal t))

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
 "covers plain safety and block scalar fallback"
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
