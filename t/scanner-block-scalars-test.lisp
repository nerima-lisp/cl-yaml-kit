;;;; t/scanner-block-scalars-test.lisp

(in-package #:cl-yaml-kit/test)

(defun block-scalar-source (text)
  (make-array (length text) :element-type 'character :initial-contents text))

(defun scanned-scalars (text)
  (let ((scanner (yaml-kit:make-scanner (block-scalar-source text)))
        (scalars nil))
    (loop for token = (yaml-kit:scanner-next-token scanner)
          while token
          when (eq (yaml-kit:token-kind token) :scalar)
            do (push (list (yaml-kit:token-value token)
                           (yaml-kit:token-style token))
                     scalars))
    (nreverse scalars)))

(defmacro scanner-block-scalar-cases (table)
  "Run the input -> scalar value/style rows in TABLE."
  `(dolist (case ,table)
     (destructuring-bind (name input expected) case
       (declare (ignore name))
       (expect (scanned-scalars input) :to-equal expected))))

(defmacro scanner-block-scalar-error-cases (table)
  "Require each input in TABLE to signal a YAML parse error."
  `(dolist (case ,table)
     (destructuring-bind (name input) case
       (declare (ignore name))
       (let ((signaled nil))
         (handler-case (progn (scanned-scalars input)
                              (setf signaled nil))
           (yaml-kit:yaml-parse-error ()
             (setf signaled t)))
         (expect signaled :to-be-truthy)))))

(defparameter *scanner-block-scalar-cases*
  `(("Example 8.1 header indicators"
     "- | # Empty header\n literal\n- >1 # Indentation indicator\n  folded\n- |+ # Chomping indicator\n keep\n\n- >1- # Both indicators\n  strip\n"
     (("literal\n" :literal)
      (" folded\n" :folded)
      ("keep\n\n" :literal)
      (" strip" :folded)))
    ("Example 8.2 detected indentation"
     ">\n  \n    \n    # detected\n"
     (("\n\n# detected\n" :folded)))
    ("Example 8.2 explicit indentation"
     "|1\n  explicit\n"
     ((" explicit\n" :literal)))
    ("Example 8.4 chomping"
     "|-\n  text\n"
     (("text" :literal)))
    ("Example 8.4 clipping"
     "|\n  text\n"
     (("text\n" :literal)))
    ("Example 8.4 keeping"
     "|+\n  text\n\n"
     (("text\n\n" :literal)))
    ("Example 8.6 empty scalar chomping"
     "|+\n"
     (("" :literal)))
    ("Example 8.7 literal scalar"
     (concatenate 'string "|\n  literal\n  " (string #\Tab) "text\n")
     (("literal\n\ttext\n" :literal)))
    ("Example 8.9 folded scalar"
     ">\n  folded\n  text\n"
     (("folded text\n" :folded)))
    ("folded scalar preserves an empty line"
     ">\n  first\n\n  second\n"
     (("first\nsecond\n" :folded)))
    ("Example 8.21 block scalar nodes"
     "literal: |2\n    value\nfolded:\n   !foo\n  >1\n value\n"
     (("value\n" :literal)
      ("value\n" :folded)))
    ("detected indentation allows a tab in content"
     (concatenate 'string ">\n \t\n detected\n")
     ((#.(format nil "~C~%detected~%" #\Tab) :folded)))))

(defparameter *scanner-block-scalar-error-cases*
  '(("Example 8.3 indicator zero" "|0\n  value\n")
    ("invalid block scalar header" "|x\n  value\n")
    ("leading content line is not indented" "- |\ntext\n")
    ("tab is not valid block indentation" "- |\n\ttext\n")
    ("leading blank line has too much indentation"
     "- |\n   \n  text\n")
    ("following text line is less indented"
     "- >\n  text\n text\n")
    ("explicit indentation is insufficient"
     "- |2\n text\n")))

(describe "block scalar scanner"
  (it "scans scalar values and styles from the specification examples"
    (scanner-block-scalar-cases *scanner-block-scalar-cases*))
  (it "rejects invalid block scalar headers and indentation"
    (scanner-block-scalar-error-cases *scanner-block-scalar-error-cases*)))
