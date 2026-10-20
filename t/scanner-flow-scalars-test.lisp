(in-package #:cl-yaml-kit/test)

(cl-weave:it-each
    ((single-quote-escape "'it''s'" (("it's" :single-quoted)))
     (double-quote-escapes "\"\\n\\t\\\\\""
       ((#.(format nil "~C~C~C" #\Newline #\Tab #\\) :double-quoted)))
     (hex-escapes "\"\\x41\\u0042\\U00000043\"" (("ABC" :double-quoted)))
     (hex-escape-for-nul "\"\\x00A\""
       ((#.(format nil "~CA" #\Nul) :double-quoted)))
     (plain-colon-in-word "a:b" (("a:b" :plain)))
     (plain-folding #.(format nil "one~%two") (("one two" :plain)))
     (quoted-line-folding
       #.(format nil "\"So does this~%  quoted scalar.\\n\"")
       ((#.(format nil "So does this quoted scalar.~%") :double-quoted)))
     (double-quoted-escaped-line-break
       #.(format nil "\"first\\~%  second\"")
       (("firstsecond" :double-quoted)))
     (escaped-break-before-closing-quote
       #.(format nil "\"value\\~%  \"")
       (("value" :double-quoted)))
     (double-quoted-escape-table
       "\"\\0\\a\\b\\e\\f\\r\\v\\N\\_\\L\\P\\/\""
       ((#.(format nil "~C~C~C~C~C~C~C~C~C~C~C/"
                   #\Nul (code-char 7) #\Backspace #\Escape #\Page #\Return #\Vt
                   (code-char #x85) (code-char #xa0) (code-char #x2028) (code-char #x2029))
         :double-quoted)))
     (quoted-empty-line-folding
       #.(format nil "\"Empty line~%  ~C~%  as a line feed\"" #\Tab)
       ((#.(format nil "Empty line~%as a line feed") :double-quoted)))
     (quoted-scalar-closes-after-folded-blanks
       #.(format nil "\"value~%  \"") (("value " :double-quoted)))
     (single-quoted-line-folding
       #.(format nil "' 1st non-empty~%~% 2nd non-empty ~% ~C3rd non-empty '" #\Tab)
       ((#.(format nil " 1st non-empty~%2nd non-empty 3rd non-empty ") :single-quoted)))
     (plain-folding-with-empty-line
       #.(format nil "plain: a~% b~%~% c")
       (("plain" :plain) (#.(format nil "a b~%c") :plain)))
     (flow-plain-colon-boundary
       #.(format nil "{~%unquoted : \"separate\",~%http://foo.com,~%omitted value:,~%}")
       (("unquoted" :plain) ("separate" :double-quoted)
        ("http://foo.com" :plain) ("omitted value" :plain))))
  "scans flow scalar case ~S"
  (name text expected)
  (declare (ignore name))
  (let ((scanner (yaml-kit::make-scanner (scanner-source text)))
        (actual nil))
    (loop for token = (yaml-kit::%scanner-next-token scanner)
          while token
          when (eq (yaml-kit:token-kind token) :scalar)
            do (push (list (yaml-kit:token-value token)
                           (yaml-kit:token-style token))
                     actual))
    (expect (nreverse actual) :to-equal expected)))

(cl-weave:it-each
    ((unknown-escape "\"\\q\"")
     (surrogate-escape "\"\\uD800\"")
     (invalid-hexadecimal-escape "\"\\x0G\"")
     (out-of-range-escape "\"\\U00110000\"")
     (unterminated-quoted-scalar "\"unterminated")
     (comment-after-quoted-scalar "\"value\"#comment")
     (unexpected-document-indicator #.(format nil "\"value~%--- ~%more\"")))
  "signals a YAML parse error for flow scalar case ~S"
  (name text)
  (declare (ignore name))
  (expect
    (lambda () (scanner-token-kinds text))
    :to-throw
    'yaml-kit:yaml-parse-error))

(cl-weave:it-each
    ((plain-word "word" ("word"))
     (plain-colon-in-word "a:b" ("a:b"))
     (mapping-key-and-value "key: value" ("key" "value"))
     (plain-space "a b" ("a b"))
     (plain-line-break #.(format nil "a~%") ("a"))
     (plain-folded-line-break #.(format nil "a~%  b") ("a b")))
  "scans plain scalar case ~S"
  (name text expected)
  (declare (ignore name))
  (let ((scanner (yaml-kit::make-scanner (scanner-source text)))
        (actual nil))
    (loop for token = (yaml-kit::%scanner-next-token scanner)
          while token
          when (eq (yaml-kit:token-kind token) :scalar)
            do (push (yaml-kit:token-value token) actual))
    (expect (nreverse actual) :to-equal expected)))

(it "rejects an under-indented tab while folding a plain scalar"
  (let* ((text (format nil "a~% ~Abad" #\Tab))
         (scanner (yaml-kit::make-scanner (scanner-source text)))
         (start (yaml-kit::sc-position scanner))
         (out (yaml-kit::make-scan-buffer))
         (end (yaml-kit::sc-position scanner))
         (leading (yaml-kit::make-scan-buffer))
         (trailing (yaml-kit::make-scan-buffer))
         (spaces (yaml-kit::make-scan-buffer)))
    (setf (yaml-kit::scanner-indent scanner) 3)
    (expect
      (lambda ()
        (yaml-kit::%scan-plain-scalar-body
         scanner start out end leading trailing spaces nil 3))
      :to-throw
      'yaml-kit:yaml-parse-error)))

(it "handles an escaped line break directly"
  (let* ((text (format nil "\\~%"))
         (scanner (yaml-kit::make-scanner (scanner-source text)))
         (out (yaml-kit::make-scan-buffer)))
    (expect (yaml-kit::%flow-escape scanner out (yaml-kit::sc-position scanner))
            :to-be nil)))

(it "folds non-break leading and trailing buffers directly"
  (let ((out (yaml-kit::make-scan-buffer))
        (leading (yaml-kit::make-scan-buffer))
        (trailing (yaml-kit::make-scan-buffer)))
    (vector-push-extend #\x leading)
    (vector-push-extend #\y trailing)
    (yaml-kit::%flow-fold out leading trailing)
    (expect (yaml-kit::scan-buffer-string out) :to-equal "xy")))

(it "stops a plain scalar at a block indicator directly"
  (let* ((text "- ")
         (scanner (yaml-kit::make-scanner (scanner-source text)))
         (start (yaml-kit::sc-position scanner))
         (out (yaml-kit::make-scan-buffer))
         (end (yaml-kit::sc-position scanner))
         (leading (yaml-kit::make-scan-buffer))
         (trailing (yaml-kit::make-scan-buffer))
         (spaces (yaml-kit::make-scan-buffer)))
    (multiple-value-bind (result blanks)
        (yaml-kit::%scan-plain-scalar-body
         scanner start out end leading trailing spaces nil 1)
      (declare (ignore result blanks))
      (expect (yaml-kit::scan-buffer-string out) :to-equal ""))))
