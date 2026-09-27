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
  ("quoted empty-line folding"
   #.(format nil "\"Empty line~%  ~C~%  as a line feed\"" #\Tab)
   ((#.(format nil "Empty line~%as a line feed") :double-quoted)))
  ("single-quoted line folding"
   #.(format nil "' 1st non-empty~%~% 2nd non-empty ~% ~C3rd non-empty '" #\Tab)
   ((#.(format nil " 1st non-empty~%2nd non-empty 3rd non-empty ") :single-quoted)))
  ("plain folding with an empty line"
   #.(format nil "plain: a~% b~%~% c")
   (("plain" :plain) (#.(format nil "a b~%c") :plain)))
  ("flow plain colon boundary"
   #.(format nil "{~%unquoted : \"separate\",~%http://foo.com,~%omitted value:,~%}")
   (("unquoted" :plain) ("separate" :double-quoted)
    ("http://foo.com" :plain) ("omitted value" :plain))))

(describe "flow scalar error cases"
  (it "rejects an unknown escape"
    (let ((text "\"\\q\""))
      (expect (handler-case
                  (let ((scanner (yaml-kit:make-scanner
                                  (make-array (length text) :element-type 'character
                                              :initial-contents text))))
                    (loop while (yaml-kit:scanner-next-token scanner)
                          finally (return nil)))
                (yaml-kit:yaml-parse-error () t)) :to-be-truthy)))
  (it "rejects a surrogate escape"
    (let ((text "\"\\uD800\""))
      (expect (handler-case
                  (let ((scanner (yaml-kit:make-scanner
                                  (make-array (length text) :element-type 'character
                                              :initial-contents text))))
                    (loop while (yaml-kit:scanner-next-token scanner)
                          finally (return nil)))
                (yaml-kit:yaml-parse-error () t)) :to-be-truthy))))
