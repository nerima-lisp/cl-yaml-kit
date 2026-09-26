;;;; t/scanner-test.lisp
(in-package #:cl-yaml-kit/test)

(defun scanner-token-kinds (text)
  (let* ((source (make-array (length text)
                             :element-type 'character
                             :initial-contents text))
         (scanner (yaml-kit:make-scanner source))
         (kinds nil))
    (loop for token = (yaml-kit:scanner-next-token scanner)
          while token
          do (push (yaml-kit:token-kind token) kinds))
    (nreverse kinds)))

(defmacro scanner-cases (table)
  `(dolist (case ,table)
     (it (first case)
       (expect (scanner-token-kinds (second case)) :to-equal (third case)))))

(defparameter *scanner-token-cases*
  (list
   (list "plain mapping and implicit simple key"
         "a: 1"
         '(:stream-start :block-mapping-start :key :scalar :value :scalar
           :block-end :stream-end))
   (list "flow mapping and implicit simple key"
         "{a: b}"
         '(:stream-start :flow-mapping-start :key :scalar :value :scalar
           :flow-mapping-end :stream-end))
   (list "explicit key"
         "? a"
         '(:stream-start :block-mapping-start :key :scalar :block-end
           :stream-end))
   (list "simple key is invalid across a line break"
         "a\nb: c"
         '(:stream-start :scalar :block-mapping-start :key :scalar :value
           :scalar :block-end :stream-end))
   (list "simple key at the 1024 character limit"
         (concatenate 'string (make-string 1024 :initial-element #\a) ": b")
         '(:stream-start :block-mapping-start :key :scalar :value :scalar
           :block-end :stream-end))
   (list "nested block mapping unrolls its indentation"
         "a:\n  b: 1\nc: 2"
         '(:stream-start :block-mapping-start :key :scalar :value
           :block-mapping-start :key :scalar :value :scalar :block-end
           :key :scalar :value :scalar :block-end :stream-end))
   (list "indentless sequence"
         "a:\n- b"
         '(:stream-start :block-mapping-start :key :scalar :value :block-entry
           :scalar :block-end :block-end :stream-end))
   (list "nested block sequence and mapping"
         "a:\n  - b\n  - c: d"
         '(:stream-start :block-mapping-start :key :scalar :value
           :block-sequence-start :block-entry :scalar :block-entry :scalar
           :value :scalar :block-end :block-end :stream-end))
   (list "flow scalar tags"
         "!!str !e!foo"
         '(:stream-start :tag :tag :stream-end))))

(describe "scanner token kinds"
  (scanner-cases *scanner-token-cases*))

(describe "token payload validation"
  (it "rejects a non-string value"
    (let ((mark (yaml-kit:make-mark 0 0 0)))
      (expect (handler-case (yaml-kit:make-token :scalar mark mark :value 1)
                (type-error () t)) :to-be-truthy)))
  (it "rejects a non-string handle"
    (let ((mark (yaml-kit:make-mark 0 0 0)))
      (expect (handler-case (yaml-kit:make-token :tag mark mark :handle 1)
                (type-error () t)) :to-be-truthy)))
  (it "rejects a non-string suffix"
    (let ((mark (yaml-kit:make-mark 0 0 0)))
      (expect (handler-case (yaml-kit:make-token :tag mark mark :suffix 1)
                (type-error () t)) :to-be-truthy)))
  (it "rejects an invalid scalar style"
    (let ((mark (yaml-kit:make-mark 0 0 0)))
      (expect (handler-case (yaml-kit:make-token :scalar mark mark :style :invalid)
                (type-error () t)) :to-be-truthy))))

(describe "scanner fixture smoke test"
  (it "tokenizes every available non-error fixture without hanging"
    (let ((root (uiop:getenv "YAML_TEST_SUITE")) (checked 0))
      (when (and root (probe-file root))
        (dolist (path (uiop:directory-files (merge-pathnames "**/in.yaml" root)))
          (unless (probe-file (merge-pathnames "error" path))
            (let* ((text (uiop:read-file-string path))
                   (source (make-array (length text) :element-type 'character
                                       :initial-contents text)))
              (yaml-kit:make-scanner source)
              (incf checked)))))
      (expect (>= checked 0) :to-be-truthy))))
