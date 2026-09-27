(in-package #:yaml-kit)

(defparameter +default-tag-directives+
  '(("!" . "!") ("!!" . "tag:yaml.org,2002:")))

(defun parser-resource-error (parser name limit actual mark)
  (declare (ignore parser))
  (error 'yaml-resource-limit-error :limit-name name :limit limit :actual actual :mark mark))

(defun append-tag-directive (parser handle prefix allow-duplicates mark)
  (when (and (not allow-duplicates)
             (assoc handle (parser-directives parser) :test #'string=))
    (parser-error (make-token :tag-directive mark mark) "found duplicate %TAG directive"))
  (push (cons handle prefix) (parser-directives parser)))

(defun process-directives (parser)
  (setf (parser-directives parser) nil (parser-version parser) nil)
  (let ((saw-directive nil))
  (loop for token = (parser-peek parser)
        while (member (token-kind token) '(:version-directive :tag-directive)) do
    (setf saw-directive t)
    (parser-next parser)
    (case (token-kind token)
      (:version-directive
       (when (parser-version parser) (parser-error token "found duplicate %YAML directive"))
       ;; Section 5.3 makes only the major version fatal: a higher minor inside
       ;; the same family is processed with a warning, so "%YAML 1.3" is a
       ;; document this reader can handle.
       (unless (= (token-major token) 1)
         (parser-error token "found incompatible YAML document"))
       (setf (parser-version parser) (cons (token-major token) (token-minor token))))
      (:tag-directive
       (append-tag-directive parser (token-handle token) (token-value token)
                             nil (token-start-mark token)))))
  (let ((mark (token-start-mark (parser-peek parser))))
    (dolist (directive +default-tag-directives+)
      (append-tag-directive parser (car directive) (cdr directive) t mark))
    saw-directive)))

(defun parser-tag-token (parser token)
  (let ((handle (token-handle token)) (suffix (token-suffix token)))
    (cond ((or (null handle) (zerop (length handle))) suffix)
          ((and (string= handle "!") (> (length suffix) 1)
                (char= (char suffix 0) #\<)
                (char= (char suffix (1- (length suffix))) #\>))
           (subseq suffix 1 (1- (length suffix))))
          (t (let ((prefix (cdr (find handle (parser-directives parser)
                                   :key #'car :test #'string= :from-end t))))
               (unless prefix (parser-error token "found undefined tag handle"))
               (concatenate 'simple-string prefix suffix))))))

(declaim (inline parser-implicit-tag-p))
(defun parser-implicit-tag-p (tag)
  (or (null tag) (zerop (length tag))))

(defun parser-empty-scalar (parser start-mark &optional end-mark anchor tag)
  (let ((end-mark (or end-mark start-mark)))
    (parser-emit parser (make-scalar-event :start-mark start-mark :end-mark end-mark
                                           :anchor anchor :tag tag :value ""
                                           :style :plain
                                           :plain-implicit-p (parser-implicit-tag-p tag)))))

(define-parser-state yaml-parser-parse-stream-start (parser)
  (let ((token (parser-next parser)))
    (unless (and token (eq (token-kind token) :stream-start))
      (parser-error token "expected stream-start token"))
    (parser-emit parser (make-stream-start-event :start-mark (token-start-mark token)
                                                 :end-mark (token-end-mark token))))
  #'yaml-parser-parse-document-start)

(define-parser-state yaml-parser-parse-document-start (parser)
  (let ((saw-directive (process-directives parser)))
    (let ((token (parser-peek parser)) (explicit nil))
    (when (and saw-directive (not (eq (token-kind token) :document-start)))
      (parser-error token "did not find expected document start"))
    (when (eq (token-kind token) :stream-end)
      (parser-emit parser (make-stream-end-event :start-mark (token-start-mark token)
                                                 :end-mark (token-end-mark token)))
      (parser-next parser)
      (return-from yaml-parser-parse-document-start nil))
    ;; A document-end marker without a preceding document is a stream gap,
    ;; not an implicit empty document (libyaml emits no document events).
    (when (eq (token-kind token) :document-end)
      (parser-next parser)
      (return-from yaml-parser-parse-document-start
        #'yaml-parser-parse-document-start))
    (when (eq (token-kind token) :document-start)
      (setf explicit t) (parser-next parser))
    (parser-emit parser (make-document-start-event :start-mark (token-start-mark token)
                         :end-mark (token-end-mark token) :explicit-p explicit
                         :version (parser-version parser)
                         :tag-directives (copy-list (parser-directives parser))))))
  #'yaml-parser-parse-document-content)

(define-parser-state yaml-parser-parse-document-content (parser)
  (let ((token (parser-peek parser)))
    (if (member (token-kind token) '(:document-start :document-end :stream-end :block-end))
        (progn (parser-empty-scalar parser (token-start-mark token)) #'yaml-parser-parse-document-end)
        (progn (parser-push parser #'yaml-parser-parse-document-end) #'yaml-parser-parse-node-block))))

(define-parser-state yaml-parser-parse-document-end (parser)
  (let ((token (parser-peek parser)) (explicit nil))
    (when (eq (token-kind token) :document-end) (setf explicit t) (parser-next parser))
    (parser-emit parser (make-document-end-event :start-mark (token-start-mark token)
                         :end-mark (token-end-mark token) :explicit-p explicit))
    (setf (parser-directives parser) nil (parser-version parser) nil)
    (let ((next (parser-peek parser)))
      (when (and explicit next
                 (= (mark-line (token-end-mark token))
                    (mark-line (token-start-mark next)))
                 (not (member (token-kind next) '(:stream-end :document-start :version-directive :tag-directive))))
        (parser-error next "did not find expected document start"))
      (when (and (not explicit) next
                 (member (token-kind next) '(:version-directive :tag-directive)))
        (parser-error next "did not find expected document end"))
      (when (and (not explicit) next
                 (= (mark-line (token-end-mark token))
                    (mark-line (token-start-mark next)))
                 (not (member (token-kind next)
                              '(:stream-end :document-start :version-directive
                                :tag-directive))))
        (parser-error next "did not find expected document start"))
      (if (eq (token-kind next) :stream-end)
          #'yaml-parser-parse-stream-end #'yaml-parser-parse-document-start))))

(define-parser-state yaml-parser-parse-stream-end (parser)
  (let ((token (parser-next parser)))
    (unless (eq (token-kind token) :stream-end) (parser-error token "expected stream-end token"))
    (parser-emit parser (make-stream-end-event :start-mark (token-start-mark token)
                                               :end-mark (token-end-mark token))))
  nil)

(defun parser-increment-depth (parser mark)
  (incf (parser-depth parser))
  (when (> (parser-depth parser) (parser-max-depth parser))
    (parser-resource-error parser "depth" (parser-max-depth parser)
                           (parser-depth parser) mark)))

(defun parser-node-properties (parser token)
  (let ((anchor nil) (tag nil) (start (token-start-mark token)) (end (token-start-mark token)))
    (loop while (member (token-kind token) '(:anchor :tag)) do
      (if (eq (token-kind token) :tag)
          (progn (when tag (parser-error token "did not find expected node content"))
                 (setf tag (parser-tag-token parser token)))
          (progn (when anchor (parser-error token "did not find expected node content"))
                 (setf anchor (token-value token))))
      (setf end (token-end-mark token))
      (parser-next parser)
      (setf token (parser-peek parser)))
    (when (and (or anchor tag) (member (token-kind token) '(:anchor :tag :alias)))
      (parser-error token "did not find expected node content"))
    (values token anchor tag start end)))

(defun parser-start-collection (parser token start anchor tag block)
  (let ((kind (token-kind token)))
    (when (member kind '(:block-sequence-start :block-mapping-start))
      (unless block (parser-error token "did not find expected node content")))
    (parser-next parser)
    (parser-increment-depth parser start)
    (case kind
      ((:flow-sequence-start :block-sequence-start)
       (parser-emit parser (make-sequence-start-event
                            :start-mark start :end-mark (token-end-mark token)
                            :anchor anchor :tag tag
                            :implicit-p (parser-implicit-tag-p tag)
                            :style (if (eq kind :flow-sequence-start) :flow :block)))
       (if (eq kind :flow-sequence-start)
           #'yaml-parser-parse-flow-sequence-first-entry
           #'yaml-parser-parse-block-sequence-entry))
      ((:flow-mapping-start :block-mapping-start)
       (parser-emit parser (make-mapping-start-event
                            :start-mark start :end-mark (token-end-mark token)
                            :anchor anchor :tag tag
                            :implicit-p (parser-implicit-tag-p tag)
                            :style (if (eq kind :flow-mapping-start) :flow :block)))
       (if (eq kind :flow-mapping-start)
           #'yaml-parser-parse-flow-mapping-first-key
           #'yaml-parser-parse-block-mapping-key)))))

(defun parser-node-content (parser token start end anchor tag block)
  (case (token-kind token)
    (:scalar
     (parser-next parser)
     (when (> (length (token-value token)) (parser-max-scalar-length parser))
       (parser-resource-error parser "scalar" (parser-max-scalar-length parser)
                              (length (token-value token)) (token-start-mark token)))
     (parser-emit parser (make-scalar-event
                          :start-mark start :end-mark (token-end-mark token)
                          :anchor anchor :tag tag :value (token-value token)
                          :style (token-style token)
                          :plain-implicit-p (or (and (null tag) (eq (token-style token) :plain))
                                                (string= (or tag "") "!"))
                          :quoted-implicit-p (and (null tag) (not (eq (token-style token) :plain)))))
     (parser-pop parser))
    ((:flow-sequence-start :flow-mapping-start :block-sequence-start :block-mapping-start)
     (parser-start-collection parser token start anchor tag block))
    (otherwise
     (if (or anchor tag)
         (progn (parser-empty-scalar parser start end anchor tag) (parser-pop parser))
         (parser-error token "did not find expected node content")))))

(defun parser-node (parser block indentless)
  (let ((token (parser-peek parser)))
    (unless token (parser-error token "expected YAML node"))
    (if (eq (token-kind token) :alias)
        (progn (parser-next parser)
               (parser-emit parser (make-alias-event :start-mark (token-start-mark token)
                                                     :end-mark (token-end-mark token)
                                                     :anchor (token-value token)))
               (parser-pop parser))
        (multiple-value-bind (content anchor tag start end)
            (parser-node-properties parser token)
          (if (and indentless (eq (token-kind content) :block-entry))
              (progn
                (parser-increment-depth parser start)
                (parser-emit parser (make-sequence-start-event
                                     :start-mark start :end-mark (token-end-mark content)
                                     :anchor anchor :tag tag
                                     :implicit-p (parser-implicit-tag-p tag) :style :block))
                #'yaml-parser-parse-indentless-sequence-entry)
              (parser-node-content parser content start end anchor tag block))))))

(define-parser-state yaml-parser-parse-node-block (parser)
  (parser-node parser t nil))
(define-parser-state yaml-parser-parse-node-block-indentless (parser)
  (parser-node parser t t))
(define-parser-state yaml-parser-parse-node-flow (parser)
  (parser-node parser nil nil))

(define-parser-state yaml-parser-parse-block-sequence-entry (parser)
  (let ((token (parser-peek parser)))
    (if (eq (token-kind token) :block-end)
        (progn (parser-next parser) (parser-emit parser (make-sequence-end-event :start-mark (token-start-mark token) :end-mark (token-end-mark token))) (decf (parser-depth parser)) (parser-pop parser))
        (progn
          (when (and (eq (token-kind token) :block-sequence-start)
                     (let* ((scanner (parser-scanner parser))
                            (head (scanner-tokens-head scanner)))
                       (and (plusp head)
                            (eq (token-kind (aref (scanner-tokens scanner) (1- head)))
                                :block-end))))
            (parser-error token "did not find expected node content"))
          (when (eq (token-kind token) :block-entry) (parser-next parser))
               ;; Only the branch that parses a node may leave a continuation for
               ;; that node to pop; an empty entry has already been consumed.
               (if (member (token-kind (parser-peek parser)) '(:block-entry :block-end))
                   (progn (parser-empty-scalar parser (token-start-mark (parser-peek parser))) #'yaml-parser-parse-block-sequence-entry)
                   (progn (parser-push parser #'yaml-parser-parse-block-sequence-entry)
                          #'yaml-parser-parse-node-block))))))

(define-parser-state yaml-parser-parse-indentless-sequence-entry (parser)
  (let ((token (parser-peek parser)))
    (if (eq (token-kind token) :block-entry)
        (progn
          (let ((mark (token-end-mark token)))
            (parser-next parser)
            (if (member (token-kind (parser-peek parser))
                        '(:block-entry :key :value :block-end))
                (progn
                  (parser-emit parser (make-scalar-event :start-mark mark :end-mark mark
                                                         :value "" :style :plain
                                                         :plain-implicit-p t))
                  #'yaml-parser-parse-indentless-sequence-entry)
                (progn
                  (parser-push parser #'yaml-parser-parse-indentless-sequence-entry)
                  #'yaml-parser-parse-node-block))))
        (progn (parser-emit parser (make-sequence-end-event :start-mark (token-start-mark token) :end-mark (token-start-mark token)))
               (decf (parser-depth parser)) (parser-pop parser)))))

(define-parser-state yaml-parser-parse-block-mapping-key (parser)
  (let ((token (parser-peek parser)))
    (cond ((eq (token-kind token) :block-entry)
           (parser-error token "did not find expected mapping key"))
        ((eq (token-kind token) :block-end)
         (parser-next parser)
         (parser-emit parser (make-mapping-end-event :start-mark (token-start-mark token) :end-mark (token-end-mark token)))
         (decf (parser-depth parser)) (parser-pop parser))
        ((eq (token-kind token) :value)
         (parser-empty-scalar parser (token-start-mark token))
         #'yaml-parser-parse-block-mapping-value)
        ((eq (token-kind token) :key)
         (let ((next-token (progn (parser-next parser) (parser-peek parser))))
           (if (member (token-kind next-token) '(:value :block-end))
               (progn (parser-empty-scalar parser (token-start-mark next-token))
                      #'yaml-parser-parse-block-mapping-value)
               (progn (parser-push parser #'yaml-parser-parse-block-mapping-value)
                      #'yaml-parser-parse-node-block-indentless))))
        (t (parser-error token "did not find expected mapping key")))))

(define-parser-state yaml-parser-parse-block-mapping-value (parser)
  (let ((token (parser-peek parser)))
    (when (eq (token-kind token) :value) (parser-next parser))
    (if (member (token-kind (parser-peek parser)) '(:key :value :block-end))
        (progn (parser-empty-scalar parser (token-start-mark (parser-peek parser))) #'yaml-parser-parse-block-mapping-key)
        (progn (parser-push parser #'yaml-parser-parse-block-mapping-key)
               #'yaml-parser-parse-node-block-indentless))))
