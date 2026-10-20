;;;; t/scanner-block-scalars-test.lisp

(in-package #:cl-yaml-kit/test)

(defun block-scalar-source (text)
  (make-array (length text) :element-type 'character :initial-contents text))

(defun scanned-scalars (text)
  (let ((scanner (yaml-kit::make-scanner (block-scalar-source text)))
        (scalars nil))
    (loop for token = (yaml-kit::%scanner-next-token scanner)
          while token
          when (eq (yaml-kit:token-kind token) :scalar)
            do (push (list (yaml-kit:token-value token)
                           (yaml-kit:token-style token))
                     scalars))
    (nreverse scalars)))

(cl-weave:it-each
    (("Example 8.1 header indicators"
      #.(format nil
                (concatenate 'string
                             "- | # Empty header~% literal~%"
                             "- >1 # Indentation indicator~%  folded~%"
                             "- |+ # Chomping indicator~% keep~%~%"
                             "- >1- # Both indicators~%  strip~%"))
      ((#.(format nil "literal~%") :literal)
       (#.(format nil " folded~%") :folded)
       (#.(format nil "keep~%~%") :literal)
       (" strip" :folded)))
     ("Example 8.2 detected indentation"
      #.(format nil ">~%  ~%    ~%    # detected~%")
      ((#.(format nil "~%~%# detected~%") :folded)))
     ("Example 8.2 explicit indentation"
      #.(format nil "|1~%  explicit~%")
      ((#.(format nil " explicit~%") :literal)))
     ("Example 8.4 chomping" #.(format nil "|-~%  text~%") (("text" :literal)))
     ("Example 8.4 clipping" #.(format nil "|~%  text~%")
      ((#.(format nil "text~%") :literal)))
     ("Example 8.4 keeping" #.(format nil "|+~%  text~%~%")
      ((#.(format nil "text~%~%") :literal)))
     ("Example 8.6 empty scalar chomping" #.(format nil "|+~%") (("" :literal)))
     ("Example 8.7 literal scalar" #.(format nil "|~%  literal~%  ~At~%" #\Tab)
      ((#.(format nil "literal~%~At~%" #\Tab) :literal)))
     ("Example 8.9 folded scalar" #.(format nil ">~%  folded~%  text~%")
      ((#.(format nil "folded text~%") :folded)))
     ("folded scalar preserves an empty line"
      #.(format nil ">~%  first~%~%  second~%")
      ((#.(format nil "first~%second~%") :folded)))
     ("Example 8.21 block scalar nodes"
      #.(format nil "literal: |2~%    value~%folded:~%   !foo~%  >1~% value~%")
      (("literal" :plain)
       (#.(format nil "  value~%") :literal)
       ("folded" :plain)
       (#.(format nil "value~%") :folded)))
     ("following text line is less indented"
      #.(format nil "- >~%  text~% text~%")
      ((#.(format nil "text~%") :folded) ("text" :plain)))
     ("detected indentation allows a tab in content"
      #.(format nil ">~% ~A~% detected~%" #\Tab)
      ((#.(format nil "~C~%detected~%" #\Tab) :folded)))
     ("document marker ends a root scalar" #.(format nil "|~%text~%---~%")
      ((#.(format nil "text~%") :literal))))
  "scans block scalar case ~S"
  (name input expected)
  (declare (ignore name))
  (expect (scanned-scalars input) :to-equal expected))

(cl-weave:it-each
    (("Example 8.3 indicator zero" #.(format nil "|0~%  value~%"))
     ("invalid block scalar header" #.(format nil "|x~%  value~%"))
     ("leading content line is not indented" #.(format nil "- |~%text~%"))
     ("tab is not valid block indentation" #.(format nil "- |~%~Atext~%" #\Tab))
     ("tab is not valid explicit block indentation"
      #.(format nil "|2~% ~Atext~%" #\Tab))
     ("explicit zero after chomping indicator" #.(format nil "|+0~%  value~%"))
     ("leading blank line has too much indentation"
      #.(format nil "- |~%   ~%  text~%"))
     ("folded leading blank line has too much indentation"
      #.(format nil "- >~%   ~%  text~%")))
  "rejects invalid block scalar case ~S"
  (name input)
  (declare (ignore name))
  (expect
    (lambda () (scanner-token-kinds input))
    :to-throw
    'yaml-kit:yaml-parse-error))
