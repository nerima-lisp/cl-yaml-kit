;;;; t/dumper-test.lisp
(in-package #:cl-yaml-kit/test)

(defun dump-to-string-with-newline (value)
  (with-output-to-string (stream)
    (yaml-kit:write-yaml value stream)))

(describe
  "dumper"
  (cl-weave:it-each
    ((nil "null\n")
     (t "true\n")
     (42 "42\n")
     ("hello" "hello\n")
     (("one" "two") "- one\n- two\n"))
    "dumps ~S as ~S"
    (value expected)
    (expect (dump-to-string-with-newline value) :to-equal expected))

  (cl-weave:it-each
    (("" "''\n")
     ("- item" "'- item'\n")
     ("? item" "'? item'\n")
     (": item" "': item'\n")
     ("-alpha" "'-alpha'\n")
     ("?alpha" "'?alpha'\n")
     (":alpha" "':alpha'\n")
     ("# comment" "'# comment'\n")
     ("key: value" "'key: value'\n")
     ("value # comment" "'value # comment'\n")
     ("a:b" "a:b\n")
     ("plain scalar" "plain scalar\n"))
    "quotes plain-scalar boundary value ~S"
    (value expected)
    (expect (dump-to-string-with-newline value) :to-equal expected)))
