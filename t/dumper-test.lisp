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
  (#.(string (code-char 1)) "\"\\x01\"")))
