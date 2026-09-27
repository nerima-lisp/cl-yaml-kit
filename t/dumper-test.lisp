;;;; t/dumper-test.lisp
(in-package #:cl-yaml-kit/test)

(defun dump-to-string-with-newline (value)
  (with-output-to-string (stream)
    (yaml-kit:write-yaml value stream)))

(defun dumper-property-generator ()
  (cl-weave:gen-recursive
   (cl-weave:gen-one-of
    (cl-weave:gen-integer :min -20 :max 20)
    (cl-weave:gen-boolean)
    (cl-weave:gen-string :min-length 0 :max-length 8
                         :alphabet "abcXYZ012 "))
   (lambda (self)
     (cl-weave:gen-one-of
      (cl-weave:gen-list self :min-length 0 :max-length 3)
      (cl-weave:gen-map
       (lambda (pairs)
         (yaml-kit:make-yaml-mapping
          (remove-duplicates pairs :test #'string= :key #'car)))
       (cl-weave:gen-list
        (cl-weave:gen-tuple
         (cl-weave:gen-string :min-length 1 :max-length 5
                              :alphabet "abcXYZ")
         self)
        :min-length 0 :max-length 3))))))

(defun dumper-roundtrip-canonical-value (value)
  (cond
    ((null value) (list :array nil))
    ((yaml-kit:yaml-null-p value) '(:yaml-null))
    ((yaml-kit:yaml-false-p value) '(:yaml-false))
    ((integerp value) (list :integer value))
    ((stringp value) (list :string value))
    ((hash-table-p value)
     (list :object
           (sort (loop for key being the hash-keys of value using (hash-value item)
                       collect (cons (dumper-roundtrip-canonical-value key)
                                     (dumper-roundtrip-canonical-value item)))
                 #'string< :key #'prin1-to-string)))
    ((yaml-kit:yaml-mapping-p value)
     (list :object
           (sort (mapcar (lambda (entry)
                           (cons (dumper-roundtrip-canonical-value (car entry))
                                 (dumper-roundtrip-canonical-value (cdr entry))))
                         (yaml-kit:yaml-mapping-entries value))
                 #'string< :key #'prin1-to-string)))
    ((vectorp value)
     (list :array (map 'list #'dumper-roundtrip-canonical-value value)))
    ((listp value)
     (list :array (mapcar #'dumper-roundtrip-canonical-value value)))
    ((typep value 'boolean) (list :boolean value))
    (t (list :other value))))

(cl-weave:it-property
 "generated values round-trip through the dumper"
 ((value (dumper-property-generator)))
 (expect (dumper-roundtrip-canonical-value
          (yaml-kit:parse (yaml-kit:emit value)))
         :to-equal
         (dumper-roundtrip-canonical-value value)))

(cl-weave:it
 "round-trips repeated shared structures"
 (let* ((shared (list "shared" 7))
        (value (list shared shared)))
   (expect (dumper-roundtrip-canonical-value
            (yaml-kit:parse (yaml-kit:emit value)))
           :to-equal
           (dumper-roundtrip-canonical-value value))))

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

(defmacro test-dumper-boundaries (cases)
  `(cl-weave:it-each ,cases
     "emits scalar boundary ~S as ~S"
     (value expected)
     (expect (dump-to-string-with-newline value)
             :to-equal (format nil "~A~%" expected))))

(test-dumper-boundaries
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
  (#.(string (code-char 1)) "\"\\x01\"")))

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

(cl-weave:it-each
    ((empty-stream
       (list (yaml-kit:make-stream-start-event)
             (yaml-kit:make-stream-end-event))
       "")
     (empty-document
       (list (yaml-kit:make-stream-start-event)
             (yaml-kit:make-document-start-event)
             (yaml-kit:make-document-end-event)
             (yaml-kit:make-stream-end-event))
       #.(format nil "---~%")))
  "emits empty event input ~S"
  (name events expected)
  (declare (ignore name))
  (expect (emit-events-to-string events) :to-equal expected))
