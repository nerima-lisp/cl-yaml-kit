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