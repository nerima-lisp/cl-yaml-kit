(in-package #:cl-yaml-kit/test)
(defparameter *invalid-token-slot-value* -1)

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
      (expect (yaml-kit::fetch-more-tokens scanner) :to-equal nil)))
  (it "clears stale simple-key candidates at line and offset limits"
  (dolist (case '((0 0 1 0) (0 0 0 1025) (0 0 0 0)))
    (destructuring-bind (mark-line mark-offset current-line current-position) case
      (let* ((scanner (yaml-kit:make-scanner (scanner-source "x")))
             (key (first (yaml-kit::scanner-simple-keys scanner))))
        (setf (yaml-kit::simple-key-possible key) t
              (yaml-kit::simple-key-required key) nil
              (yaml-kit::simple-key-mark key)
              (yaml-kit:make-mark mark-line 0 mark-offset)
              (yaml-kit::scanner-line scanner) current-line
              (yaml-kit::scanner-pos scanner) current-position)
        (yaml-kit::stale-simple-keys scanner)
        (expect (yaml-kit::simple-key-possible key)
                :to-be (if (equal case '(0 0 0 0)) t nil)))))
  (let* ((scanner (yaml-kit:make-scanner (scanner-source "x")))
         (key (first (yaml-kit::scanner-simple-keys scanner))))
    (setf (yaml-kit::simple-key-possible key) t
          (yaml-kit::simple-key-required key) nil)
    (yaml-kit::remove-simple-key scanner)
    (expect (yaml-kit::simple-key-possible key) :to-be-falsy))))

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
                           (lambda () (yaml-kit:make-token :scalar mark mark :style :invalid))
                           (lambda () (yaml-kit::%make-token
                                       :scalar mark mark
                                       :value *invalid-token-slot-value*))
                           (lambda () (yaml-kit::%make-token
                                       :tag mark mark
                                       :handle *invalid-token-slot-value*))
                           (lambda () (yaml-kit::%make-token
                                       :tag mark mark
                                       :suffix *invalid-token-slot-value*))
                           (lambda () (yaml-kit::%make-token
                                       :scalar mark mark
                                       :style *invalid-token-slot-value*))))
        (expect (handler-case (funcall thunk)
                  (type-error () t))
                :to-be-truthy))))
  (it "validates token kind marks and numeric metadata"
    (let ((mark (yaml-kit:make-mark 0 0 0)))
      (dolist (thunk (list (lambda () (yaml-kit:make-token 1 mark mark))
                           (lambda () (yaml-kit:make-token :scalar 1 mark))
                           (lambda () (yaml-kit:make-token :scalar mark 1))
                           (lambda () (yaml-kit:make-token :version-directive mark mark :major t))
                           (lambda () (yaml-kit:make-token :version-directive mark mark :minor t))))
        (expect (handler-case (funcall thunk) (type-error () t)) :to-be-truthy))))
  (it "constructs tokens with each valid payload variant"
    (let* ((start-mark (yaml-kit:make-mark 1 2 3))
           (end-mark (yaml-kit:make-mark 1 8 9))
           (scalar (yaml-kit:make-token :scalar start-mark end-mark
                                        :value "quoted" :style :double-quoted))
           (tag (yaml-kit:make-token :tag start-mark end-mark
                                     :handle "!e!" :suffix "item"))
           (version (yaml-kit:make-token :version-directive start-mark end-mark
                                         :major 1 :minor 2)))
      (expect (list (list (yaml-kit:token-kind scalar)
                          (yaml-kit:token-value scalar)
                          (yaml-kit:token-handle scalar)
                          (yaml-kit:token-suffix scalar)
                          (yaml-kit:token-style scalar)
                          (yaml-kit:token-major scalar)
                          (yaml-kit:token-minor scalar)
                          (eq (yaml-kit:token-start-mark scalar) start-mark)
                          (eq (yaml-kit:token-end-mark scalar) end-mark))
                    (list (yaml-kit:token-kind tag)
                          (yaml-kit:token-value tag)
                          (yaml-kit:token-handle tag)
                          (yaml-kit:token-suffix tag)
                          (yaml-kit:token-style tag)
                          (yaml-kit:token-major tag)
                          (yaml-kit:token-minor tag)
                          (eq (yaml-kit:token-start-mark tag) start-mark)
                          (eq (yaml-kit:token-end-mark tag) end-mark))
                    (list (yaml-kit:token-kind version)
                          (yaml-kit:token-value version)
                          (yaml-kit:token-handle version)
                          (yaml-kit:token-suffix version)
                          (yaml-kit:token-style version)
                          (yaml-kit:token-major version)
                          (yaml-kit:token-minor version)
                          (eq (yaml-kit:token-start-mark version) start-mark)
                          (eq (yaml-kit:token-end-mark version) end-mark)))
              :to-equal (quote ((:scalar "quoted" nil nil :double-quoted nil nil t t)
                                (:tag nil "!e!" "item" nil nil nil t t)
                                (:version-directive nil nil nil nil 1 2 t t)))))))
