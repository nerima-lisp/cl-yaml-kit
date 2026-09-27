(in-package #:cl-yaml-kit/test)

(defmacro flow-scalar-cases (&body cases)
  `(progn
     ,@(mapcar (lambda (case)
                 `(it ,(first case)
                    (let* ((text (second ',case))
                           (source (make-array (length text)
                                               :element-type 'character
                                               :initial-contents text))
                           (scanner (yaml-kit:make-scanner source))
                           (actual nil))
                      (loop for token = (yaml-kit:scanner-next-token scanner)
                            while token do
                              (when (eq (yaml-kit:token-kind token) :scalar)
                                (push (list (yaml-kit:token-value token)
                                            (yaml-kit:token-style token)) actual)))
                      (expect (nreverse actual) :to-equal (third ',case)))))
               cases)))

(describe "flow scalar scanner"
  (flow-scalar-cases
    ("single quote escape" "'it''s'" (("it's" :single-quoted)))
    ("double quote escapes" "\"\\n\\t\\\\\"" ((#.(format nil "~C~C~C" #\Newline #\Tab #\\) :double-quoted)))
    ("hex escapes" "\"\\x41\\u0042\\U00000043\"" (("ABC" :double-quoted)))
    ("hex escape for NUL" "\"\\x00A\""
     ((#.(format nil "~CA" #\Nul) :double-quoted)))
    ("plain colon in word" "a:b" (("a:b" :plain)))
    ("plain folding" #.(format nil "one~%two") (("one two" :plain)))
    ("quoted line folding"
     #.(format nil "\"So does this~%  quoted scalar.\\n\"")
     ((#.(format nil "So does this quoted scalar.~%") :double-quoted)))
    ("double-quoted escaped line break"
     #.(format nil "\"first\\~%  second\"")
     (("firstsecond" :double-quoted)))
    ("escaped break ends before the closing quote"
     #.(format nil "\"value\\~%  \"")
     (("value" :double-quoted)))
    ("double-quoted escape table"
     "\"\\0\\a\\b\\e\\f\\r\\v\\N\\_\\L\\P\\/\""
     ((#.(format nil "~C~C~C~C~C~C~C~C~C~C~C/"
                 #\Nul (code-char 7) #\Backspace #\Escape #\Page #\Return #\Vt
                 (code-char #x85) (code-char #xa0) (code-char #x2028) (code-char #x2029))
        :double-quoted)))
    ("quoted empty-line folding"
     #.(format nil "\"Empty line~%  ~C~%  as a line feed\"" #\Tab)
     ((#.(format nil "Empty line~%as a line feed") :double-quoted)))
    ("quoted scalar closes after folded blanks"
     #.(format nil "\"value~%  \"")
     (("value " :double-quoted)))
    ("single-quoted line folding"
     #.(format nil "' 1st non-empty~%~% 2nd non-empty ~% ~C3rd non-empty '" #\Tab)
     ((#.(format nil " 1st non-empty~%2nd non-empty 3rd non-empty ") :single-quoted)))
    ("plain folding with an empty line"
     #.(format nil "plain: a~% b~%~% c")
     (("plain" :plain) (#.(format nil "a b~%c") :plain)))
    ("flow plain colon boundary"
     #.(format nil "{~%unquoted : \"separate\",~%http://foo.com,~%omitted value:,~%}")
     (("unquoted" :plain) ("separate" :double-quoted)
      ("http://foo.com" :plain) ("omitted value" :plain)))))

(describe "flow scalar error cases"
  (macrolet ((flow-error-cases (&body cases)
               `(progn
                  ,@(mapcar (lambda (case)
                              `(it ,(first case)
                                 (let ((text ,(second case)))
                                   (expect (handler-case
                                               (let ((scanner (yaml-kit:make-scanner
                                                               (make-array (length text)
                                                                           :element-type 'character
                                                                           :initial-contents text))))
                                                 (loop while (yaml-kit:scanner-next-token scanner)
                                                       finally (return nil)))
                                             (yaml-kit:yaml-parse-error () t))
                                           :to-be-truthy))))
                            cases))))
  (flow-error-cases
      ("rejects an unknown escape" "\"\\q\"")
      ("rejects a surrogate escape" "\"\\uD800\"")
      ("rejects an invalid hexadecimal escape" "\"\\x0G\"")
      ("rejects an out-of-range escape" "\"\\U00110000\"")
      ("rejects an unterminated quoted scalar" "\"unterminated")
      ("rejects a comment after a quoted scalar" "\"value\"#comment")
      ("rejects an unexpected document indicator"
       (format nil "\"value~%--- ~%more\""))
      )))
(describe "plain scalar scanner"
  (it "covers direct and folded plain scalar branches"
    (dolist (case '(("word" ("word"))
                    ("a:b" ("a:b"))
                    ("key: value" ("key" "value"))
                    ("a b" ("a b"))
                    (#.(format nil "a~%") ("a"))
                    (#.(format nil "a~%  b") ("a b"))))
      (destructuring-bind (text expected) case
        (let ((scanner (yaml-kit:make-scanner
                        (make-array (length text)
                                    :element-type 'character
                                    :initial-contents text)))
              (actual nil))
          (loop for token = (yaml-kit:scanner-next-token scanner)
                while token
                when (eq (yaml-kit:token-kind token) :scalar)
                  do (push (yaml-kit:token-value token) actual))
          (expect (nreverse actual) :to-equal expected))))))

(it "rejects an under-indented tab while folding a plain scalar"
  (let* ((text (format nil "a~% ~Abad" #\Tab))
         (scanner (yaml-kit:make-scanner
                   (make-array (length text) :element-type 'character
                               :initial-contents text)))
         (start (yaml-kit::sc-mark scanner))
         (out (yaml-kit::make-scan-buffer))
         (end (yaml-kit::sc-mark scanner))
         (leading (yaml-kit::make-scan-buffer))
         (trailing (yaml-kit::make-scan-buffer))
         (spaces (yaml-kit::make-scan-buffer)))
    (setf (yaml-kit::scanner-indent scanner) 3)
    (expect (handler-case
                (yaml-kit::%scan-plain-scalar-body
                 scanner start out end leading trailing spaces nil 3)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)))

(it "handles an escaped line break directly"
  (let* ((text (format nil "\\~%"))
         (scanner (yaml-kit:make-scanner
                   (make-array (length text) :element-type 'character
                               :initial-contents text)))
         (out (yaml-kit::make-scan-buffer)))
    (expect (yaml-kit::%flow-escape scanner out (yaml-kit::sc-mark scanner))
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
         (scanner (yaml-kit:make-scanner
                   (make-array (length text) :element-type 'character
                               :initial-contents text)))
         (start (yaml-kit::sc-mark scanner))
         (out (yaml-kit::make-scan-buffer))
         (end (yaml-kit::sc-mark scanner))
         (leading (yaml-kit::make-scan-buffer))
         (trailing (yaml-kit::make-scan-buffer))
         (spaces (yaml-kit::make-scan-buffer)))
    (multiple-value-bind (result blanks)
        (yaml-kit::%scan-plain-scalar-body
         scanner start out end leading trailing spaces nil 1)
      (declare (ignore result blanks))
      (expect (yaml-kit::scan-buffer-string out) :to-equal ""))))
