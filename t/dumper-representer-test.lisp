;;;; t/dumper-representer-test.lisp
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
  "emits boundary strings through the representer ~S as ~S"
  (value expected)
  (expect value :to-emit-as (format nil "~A~%" expected)))

(cl-weave:it-each
    ((empty-string "")
     (leading-space " lead")
     (trailing-space "trail ")
     (both-edge-spaces " edge ")
     (space-only " "))
  "reads back emitted scalar boundary value ~S"
  (name value)
  (declare (ignore name))
  (expect value :to-round-trip t))

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
  (expect value :to-emit-as expected))

(cl-weave:it
 "dumps a hash table and preserves an explicit document marker"
 (let ((table (make-hash-table :test #'equal)))
   (setf (gethash "answer" table) 42)
   (expect (yaml-kit:emit table :explicit-document-start t)
           :to-equal #.(format nil "---~%answer: 42~%"))))

(cl-weave:it
 "dumps an empty mapping"
 (expect (yaml-kit:make-yaml-mapping nil)
         :to-emit-as #.(format nil "{}~%")))

(cl-weave:it
 "supports event lists and write-yaml"
 (let ((events (list (yaml-kit:make-stream-start-event)
                     (yaml-kit:make-document-start-event)
                     (yaml-kit:make-scalar-event :value "ok")
                     (yaml-kit:make-document-end-event)
                     (yaml-kit:make-stream-end-event))))
   (expect (yaml-kit:emit-events events) :to-equal #.(format nil "ok~%"))
   (with-output-to-string (stream)
     (expect (yaml-kit:write-yaml 7 stream) :to-equal 7)
     (expect (get-output-stream-string stream) :to-equal #.(format nil "7~%")))))

(cl-weave:it-each
    ((plain-analysis "plain" nil :plain)
     (flow-analysis "a,b" t :single-quoted)
     (line-break-analysis #.(format nil "a~%b") nil :plain)
     (control-analysis #.(string (code-char 1)) nil :double-quoted)
     (unicode-analysis #.(string (code-char #x2028)) nil :plain))
  "selects styles without a string tag for plain-request values ~S"
  (name value flow expected-style)
  (declare (ignore name))
  (expect (yaml-kit::%scalar-style value :plain flow) :to-equal expected-style))
