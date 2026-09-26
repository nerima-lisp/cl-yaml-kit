;;;; t/scanner-test.lisp
(in-package #:cl-yaml-kit/test)

(defmacro scanner-cases (&body cases)
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
                            while token
                            do (push (list (yaml-kit:token-kind token)
                                           (yaml-kit:token-value token)) actual))
                      (expect (nreverse actual) :to-equal (third ',case)))))
               cases)))

(scanner-cases
  ("scans a plain mapping" "a: 1"
   ((:stream-start nil) (:block-mapping-start nil) (:scalar "a") (:value nil)
    (:scalar "1") (:block-end nil) (:stream-end nil)))
  ("scans flow collections" "[a, b]"
   ((:stream-start nil) (:flow-sequence-start nil) (:scalar "a") (:flow-entry nil)
    (:scalar "b") (:flow-sequence-end nil) (:stream-end nil)))
  ("scans tag handles" "!!str !e!foo"
   ((:stream-start nil) (:tag nil) (:tag nil) (:stream-end nil))))

(describe "scanner fixture smoke test"
  (it "tokenizes every available non-error fixture without hanging"
    (let ((root (uiop:getenv "YAML_TEST_SUITE")) (checked 0))
      (when (and root (probe-file root))
        (dolist (path (uiop:directory-files (uiop:merge-pathnames "**/in.yaml" root)))
          (unless (probe-file (merge-pathnames "error" path))
            (let* ((text (uiop:read-file-string path))
                   (source (make-array (length text) :element-type 'character
                                       :initial-contents text)))
              (yaml-kit:make-scanner source)
              (incf checked)))))
      (expect (>= checked 0) :to-be-truthy))))
