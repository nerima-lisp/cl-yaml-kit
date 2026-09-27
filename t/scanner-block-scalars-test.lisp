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

;; The reader in use does not expand a single backslash inside a string, so every
;; line break here comes from a ~% directive rather than a "\n" escape.  Each row
;; is (name input expected); expected is a list of (value style) pairs.
(defun scanner-block-scalar-rows ()
  (list
   (list "Example 8.1 header indicators"
         (format nil "- | # Empty header~% literal~%- >1 # Indentation indicator~%  folded~%- |+ # Chomping indicator~% keep~%~%- >1- # Both indicators~%  strip~%")
         (list (list (format nil "literal~%") :literal)
               (list (format nil " folded~%") :folded)
               (list (format nil "keep~%~%") :literal)
               (list " strip" :folded)))
   (list "Example 8.2 detected indentation"
         (format nil ">~%  ~%    ~%    # detected~%")
         (list (list (format nil "~%~%# detected~%") :folded)))
   (list "Example 8.2 explicit indentation"
         (format nil "|1~%  explicit~%")
         (list (list (format nil " explicit~%") :literal)))
   (list "Example 8.4 chomping"
         (format nil "|-~%  text~%")
         (list (list "text" :literal)))
   (list "Example 8.4 clipping"
         (format nil "|~%  text~%")
         (list (list (format nil "text~%") :literal)))
   (list "Example 8.4 keeping"
         (format nil "|+~%  text~%~%")
         (list (list (format nil "text~%~%") :literal)))
   (list "Example 8.6 empty scalar chomping"
         (format nil "|+~%")
         (list (list "" :literal)))
   (list "Example 8.7 literal scalar"
         (format nil "|~%  literal~%  ~At~%" #\Tab)
         (list (list (format nil "literal~%~At~%" #\Tab) :literal)))
   (list "Example 8.9 folded scalar"
         (format nil ">~%  folded~%  text~%")
         (list (list (format nil "folded text~%") :folded)))
   (list "folded scalar preserves an empty line"
         (format nil ">~%  first~%~%  second~%")
         (list (list (format nil "first~%second~%") :folded)))
   (list "Example 8.21 block scalar nodes"
         (format nil "literal: |2~%    value~%folded:~%   !foo~%  >1~% value~%")
         (list (list "literal" :plain)
               (list (format nil "  value~%") :literal)
               (list "folded" :plain)
               (list (format nil "value~%") :folded)))
   ;; A content line indented less than the scalar ends it, and the rest of
   ;; the line becomes a node of its own.
   (list "following text line is less indented"
         (format nil "- >~%  text~% text~%")
         (list (list (format nil "text~%") :folded)
               (list "text" :plain)))
   (list "detected indentation allows a tab in content"
         (format nil ">~% ~A~% detected~%" #\Tab)
         (list (list (format nil "~C~%detected~%" #\Tab) :folded)))))

(defun scanner-block-scalar-error-rows ()
  (list
   (list "Example 8.3 indicator zero" (format nil "|0~%  value~%"))
   (list "invalid block scalar header" (format nil "|x~%  value~%"))
   (list "leading content line is not indented" (format nil "- |~%text~%"))
   (list "tab is not valid block indentation" (format nil "- |~%~Atext~%" #\Tab))
   (list "leading blank line has too much indentation" (format nil "- |~%   ~%  text~%"))
   (list "folded leading blank line has too much indentation" (format nil "- >~%   ~%  text~%"))))

(describe "block scalar scanner"
  (it "scans scalar values and styles from the specification examples"
    (dolist (row (scanner-block-scalar-rows))
      (destructuring-bind (name input expected) row
        (declare (ignore name))
        (expect (scanned-scalars input) :to-equal expected))))
  (it "rejects invalid block scalar headers and indentation"
    (dolist (row (scanner-block-scalar-error-rows))
      (destructuring-bind (name input) row
        (declare (ignore name))
        (let ((signaled nil))
          (handler-case (progn (scanned-scalars input) (setf signaled nil))
            (yaml-kit:yaml-parse-error () (setf signaled t)))
          (expect signaled :to-be-truthy))))))
