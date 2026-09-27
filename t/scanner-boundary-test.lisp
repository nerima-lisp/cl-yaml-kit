(in-package #:cl-yaml-kit/test)

(describe "scanner token errors"
  (it "rejects a value indicator that would close a key from a previous line"
    (expect (handler-case
                (progn
                  (let ((scanner (yaml-kit:make-scanner
                                  (scanner-source (format nil "a~%b: c")))))
                    (loop while (yaml-kit:scanner-next-token scanner)))
                  nil)
              (yaml-kit:yaml-parse-error () t))
            :to-be-truthy))
  (it "rejects a flow entry outside a flow collection"
    (expect (handler-case (scanner-token-kinds ",")
              (yaml-kit:yaml-parse-error () t)) :to-be-truthy))
  (it "rejects a flow entry followed by a comment marker"
    (expect (handler-case (scanner-token-kinds "[item,#]")
              (yaml-kit:yaml-parse-error () t)) :to-be-truthy))
  (it "rejects a flow entry when called outside flow context"
    (let ((scanner (yaml-kit:make-scanner (scanner-source ","))))
      (expect (handler-case (yaml-kit::fetch-flow-entry scanner)
                (yaml-kit:yaml-parse-error () t)) :to-be-truthy)))
  (it "rejects a required simple key when it is removed"
    (let* ((scanner (yaml-kit:make-scanner (scanner-source "key")))
           (key (first (yaml-kit::scanner-simple-keys scanner))))
      (setf (yaml-kit::simple-key-possible key) t
            (yaml-kit::simple-key-required key) t
            (yaml-kit::simple-key-mark key) (yaml-kit:make-mark 0 0 0))
      (expect (handler-case (yaml-kit::remove-simple-key scanner)
                (yaml-kit:yaml-parse-error () t)) :to-be-truthy)))
  (it "reports a token queue resource limit"
    (let ((scanner (yaml-kit:make-scanner (scanner-source ""))))
      (dotimes (i 65) (vector-push-extend nil (yaml-kit::scanner-tokens scanner)))
      (setf (yaml-kit::scanner-tokens-head scanner) 65
            (yaml-kit::scanner-tokens-parsed scanner) 65)
      (expect (handler-case (yaml-kit::fetch-more-tokens scanner)
                (yaml-kit:yaml-resource-limit-error () t)) :to-be-truthy)))
  (it "skips a BOM before the first token"
    (expect (scanner-token-kinds (format nil "~Cvalue" #\UFEFF))
            :to-equal '(:stream-start :scalar :stream-end)))
  (it "does not fetch after stream end is produced"
    (let ((scanner (yaml-kit:make-scanner (scanner-source ""))))
      (setf (yaml-kit::scanner-stream-end-produced scanner) t)
      (expect (yaml-kit::fetch-more-tokens scanner) :to-equal nil))))

(describe "token payload validation"
  (it "identifies JSON-like node endings"
    (let ((mark (yaml-kit:make-mark 0 0 0)))
      (dolist (case '((:scalar :single-quoted t) (:scalar :double-quoted t)
                      (:flow-sequence-end nil t) (:flow-mapping-end nil t)
                      (:scalar :plain nil) (:alias nil nil)))
        (destructuring-bind (kind style expected) case
          (expect (yaml-kit::token-ends-json-like-node-p
                   (yaml-kit:make-token kind mark mark :style style)) :to-equal expected)))))
  (it "rejects invalid token payloads"
    (let ((mark (yaml-kit:make-mark 0 0 0)))
      (dolist (thunk (list (lambda () (yaml-kit:make-token :scalar mark mark :value 1))
                           (lambda () (yaml-kit:make-token :tag mark mark :handle 1))
                           (lambda () (yaml-kit:make-token :tag mark mark :suffix 1))
                           (lambda () (yaml-kit:make-token :scalar mark mark :style :invalid))))
        (expect (handler-case (funcall thunk) (type-error () t)) :to-be-truthy))))
  (it "validates token kind marks and numeric metadata"
    (let ((mark (yaml-kit:make-mark 0 0 0)))
      (dolist (thunk (list (lambda () (yaml-kit:make-token 1 mark mark))
                           (lambda () (yaml-kit:make-token :scalar 1 mark))
                           (lambda () (yaml-kit:make-token :scalar mark 1))
                           (lambda () (yaml-kit:make-token :version-directive mark mark :major t))
                           (lambda () (yaml-kit:make-token :version-directive mark mark :minor t))))
        (expect (handler-case (funcall thunk) (type-error () t)) :to-be-truthy)))))
