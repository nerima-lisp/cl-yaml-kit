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
  ("double quote escapes" "\"\\n\\t\\\\\"" ((,(format nil "~C~C~C" #\Newline #\Tab #\\) :double-quoted)))
  ("hex escapes" "\"\\x41\\u0042\\U00000043\"" (("ABC" :double-quoted)))
  ("plain colon in word" "a:b" (("a:b" :plain)))
  ("plain folding" "one\ntwo" (("one two" :plain))))

(describe "flow scalar error cases"
  (it "rejects an unknown escape"
    (let ((text "\"\\q\""))
      (expect (handler-case
                  (progn (yaml-kit:make-scanner
                          (make-array (length text) :element-type 'character
                                      :initial-contents text)) nil)
                (yaml-kit:yaml-parse-error () t)) :to-be-truthy)))
  (it "rejects a surrogate escape"
    (let ((text "\"\\uD800\""))
      (expect (handler-case
                  (progn (yaml-kit:make-scanner
                          (make-array (length text) :element-type 'character
                                      :initial-contents text)) nil)
                (yaml-kit:yaml-parse-error () t)) :to-be-truthy))))
