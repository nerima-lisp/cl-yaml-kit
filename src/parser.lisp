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
