(in-package #:cl-yaml-kit/test)

(cl-weave:it-each
    ((leading-comment "# leading
value
" ((0 . 9)))
     (directive-comment "%YAML 1.2 # version
---
value
" ((10 . 19)))
     (reserved-directive-comments
       #.(format nil "%FOO arg#not # reserved~%# gap~%---~%value~%")
       ((13 . 23) (24 . 29)))
     (standalone-comment "key: one
# standalone
other: two
" ((9 . 21)))
     (inline-comment "key: value # inline
" ((11 . 19)))
     (trailing-comment-at-eof "key: value # eof" ((11 . 16)))
     (multiple-documents "# first
---
first: value # one
...
# second
---
second: value # two" ((0 . 7) (25 . 30) (35 . 43) (62 . 67)))
     (flow-collection-comments "[one, # first
 two] # tail
" ((6 . 13) (20 . 26)))
     (single-quoted-hash #.(format nil "single: ~Ca # b~C~%" (code-char 39) (code-char 39)) nil)
     (double-quoted-hash "double: \"a # b\"
" nil)
     (literal-block-hash "literal: |-
  # body
  text
" nil)
     (folded-block-hash "folded: >-
  # body
  text
" nil)
     (block-scalar-header-comment "value: | # header
  # body
  text
" ((9 . 17)))
     (plain-scalar-hash-without-separation "value: foo#bar # actual
" ((15 . 23)))
     (crlf-comment #.(format nil "key: value # crlf~C~C" (code-char 13) (code-char 10)) ((11 . 17)))
     (non-ascii-prefix "日本語 # comment" ((4 . 13))))
  "reports comment spans for ~S"
  (name input expected)
  (declare (ignore name))
  (expect (yaml-kit:comment-spans input) :to-equal expected))

(cl-weave:it-each
    ((simple-base-string
       #.(coerce "value # tail" (quote simple-base-string))
       t)
     (adjustable-character-string
       #.(make-array 12 :element-type 'character :adjustable t
                     :fill-pointer 12 :initial-contents "value # tail")
       nil))
  "recognizes string input types"
  (name input simple-base-p)
  (declare (ignore name))
  (expect (stringp input) :to-be-truthy)
  (expect (typep input 'simple-base-string) :to-equal simple-base-p))


(cl-weave:it-property
 "adding a legal inline comment preserves generated scalar values"
 ((text (cl-weave:gen-string :min-length 1
                             :max-length 16
                             :alphabet "abcXYZ")))
 (expect (yaml-kit:parse text)
         :to-equal
         (yaml-kit:parse (format nil "~A # legal comment" text))))
(it "enforces :max-input-length for comment scans"
  (expect (lambda () (yaml-kit:comment-spans "a" :max-input-length 0))
          :to-signal-loader-resource-limit
          '(:name "input length" :limit 0 :actual 1)))

(cl-weave:it-each
    ((simple-base-string
       #.(coerce "value # tail" (quote simple-base-string))
       ((6 . 12)))
     (adjustable-character-string
       #.(make-array 12 :element-type 'character :adjustable t
                     :fill-pointer 12 :initial-contents "value # tail")
       ((6 . 12))))
  "parses and scans ~S input consistently"
  (name input expected-spans)
  (declare (ignore name))
  (expect (yaml-kit:parse input) :to-equal "value")
  (expect (yaml-kit:comment-spans input) :to-equal expected-spans))
