;;;; t/scanner-test.lisp
(in-package #:cl-yaml-kit/test)

(defun scanner-source (text)
  (make-array (length text) :element-type 'character :initial-contents text))

(defun scanner-token-kinds (text)
  (let ((scanner (yaml-kit:make-scanner (scanner-source text)))
        (kinds nil))
    (loop for token = (yaml-kit:scanner-next-token scanner)
          while token
          do (push (yaml-kit:token-kind token) kinds))
    (nreverse kinds)))

(defmacro scanner-cases (table)
  `(dolist (case ,table)
     (it (first case)
       (expect (scanner-token-kinds (second case)) :to-equal (third case)))))

(describe "scanner character classes"
  (it "expands character productions and defines their predicates"
    (let ((expansion
            (macroexpand-1
             '(yaml-kit::%define-character-production probe-character "AZ"))))
      (expect (first expansion) :to-equal 'progn))
    (eval '(yaml-kit::%define-character-production probe-character-eval "AZ"))
    (let ((predicate (find-symbol "YAML-PROBE-CHARACTER-EVAL-P" *package*)))
      (expect (funcall (symbol-function predicate) #\A) :to-be-truthy)
      (expect (funcall (symbol-function predicate) #\B) :to-equal nil)
      (expect (funcall (symbol-function predicate) 1) :to-equal nil))
    (eval '(yaml-kit::%define-character-production probe-character-list
             '(#\A #\Z)))
    (let ((predicate (find-symbol "YAML-PROBE-CHARACTER-LIST-P" *package*)))
      (expect (funcall (symbol-function predicate) #\Z) :to-be-truthy)
      (expect (funcall (symbol-function predicate) #\B) :to-equal nil)))
  (dolist (case '(("digit accepts ASCII" yaml-kit::sc-digit-p "7" t)
                  ("digit rejects fullwidth" yaml-kit::sc-digit-p "７" nil)
                  ("hex accepts ASCII" yaml-kit::sc-hex-p "F" t)
                  ("hex rejects fullwidth" yaml-kit::sc-hex-p "Ｆ" nil)
                  ("alpha accepts ASCII" yaml-kit::sc-alpha-p "A" t)
                  ("alpha accepts ASCII digit" yaml-kit::sc-alpha-p "7" t)
                  ("alpha accepts lowercase" yaml-kit::sc-alpha-p "a" t)
                  ("alpha rejects Unicode digit" yaml-kit::sc-alpha-p "あ" nil)))
    (destructuring-bind (name predicate text expected) case
      (it name
        (let ((scanner (yaml-kit:make-scanner (scanner-source text))))
          (expect (funcall predicate scanner) :to-equal expected))))))

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
   ;; A line break folds into the plain scalar, so nothing on the next line can
   ;; close a simple key opened on the previous one.
   (list "plain scalar folds a line break"
         (format nil "a~%b")
         '(:stream-start :scalar :stream-end))
   (list "simple key at the 1024 character limit"
         (concatenate 'string (make-string 1024 :initial-element #\a) ": b")
         '(:stream-start :block-mapping-start :key :scalar :value :scalar
           :block-end :stream-end))
   (list "nested block mapping unrolls its indentation"
         (format nil "a:~%  b: 1~%c: 2")
         '(:stream-start :block-mapping-start :key :scalar :value
           :block-mapping-start :key :scalar :value :scalar :block-end
           :key :scalar :value :scalar :block-end :stream-end))
   ;; An indentless sequence shares the indentation of the mapping that owns it,
   ;; so the mapping contributes the only BLOCK-END.
   (list "indentless sequence"
         (format nil "a:~%- b")
         '(:stream-start :block-mapping-start :key :scalar :value :block-entry
           :scalar :block-end :stream-end))
   ;; "- c: d" is a compact mapping, and a compact mapping rolls its own
   ;; indentation, so a nested BLOCK-MAPPING-START precedes its key.
   (list "nested block sequence and mapping"
         (format nil "a:~%  - b~%  - c: d")
         '(:stream-start :block-mapping-start :key :scalar :value
           :block-sequence-start :block-entry :scalar :block-entry
           :block-mapping-start :key :scalar :value :scalar :block-end
           :block-end :block-end :stream-end))
   (list "flow scalar tags"
         "!!str !e!foo"
         '(:stream-start :tag :tag :stream-end))))

(defparameter *scanner-block-indicator-cases*
  (list
   (list "block plain scalar begins with comma" ",word"
         '(:stream-start :scalar :stream-end))))

(describe "scanner token kinds"
  (scanner-cases *scanner-token-cases*)
  (scanner-cases *scanner-block-indicator-cases*))

(describe "scanner token errors"
  (it "rejects a value indicator that would close a key from a previous line"
    (expect (handler-case
                (progn
                  (let ((scanner (yaml-kit:make-scanner
                                  (scanner-source (format nil "a~%b: c")))))
                    (loop while (yaml-kit:scanner-next-token scanner)))
                  nil)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy)))

(describe "token payload validation"
  (it "identifies JSON-like node endings"
    (let ((mark (yaml-kit:make-mark 0 0 0)))
      (dolist (case '((:scalar :single-quoted t)
                      (:scalar :double-quoted t)
                      (:flow-sequence-end nil t)
                      (:flow-mapping-end nil t)
                      (:scalar :plain nil)
                      (:alias nil nil)))
        (destructuring-bind (kind style expected) case
          (expect (yaml-kit::token-ends-json-like-node-p
                   (yaml-kit:make-token kind mark mark :style style))
                  :to-equal expected)))))
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

(defun suite-case-inputs (root)
  "Every IN.YAML of a suite case directory, one level below ROOT.
DIRECTORY-FILES with a \"**/in.yaml\" pattern also reaches into .git, whose
index and pack files are not text."
  (loop for directory in (directory (merge-pathnames "*/" root))
        for input = (merge-pathnames "in.yaml" directory)
        when (probe-file input)
          collect input))

(defun fixture-error-p (input)
  (probe-file (merge-pathnames "error" (make-pathname :name nil :type nil :defaults input))))

(describe "scanner fixture smoke test"
  (it "tokenizes every available non-error fixture without hanging"
    (let ((root (uiop:getenv "YAML_TEST_SUITE")))
      (when (and root (probe-file root))
        ;; 255 of the suite's 333 cases are documents; the other 78 carry an
        ;; "error" marker, so tokenizing them is not expected to succeed.
          (let* ((inputs (suite-case-inputs root))
               (documents (remove-if #'fixture-error-p inputs)))
          (dolist (path documents)
            (let ((scanner (yaml-kit:make-scanner
                            (scanner-source (uiop:read-file-string path)))))
              (loop while (yaml-kit:scanner-next-token scanner))))
          (expect (length inputs) :to-be 333)
          (expect (length documents) :to-be 255))))))
