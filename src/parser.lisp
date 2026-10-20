(in-package #:yaml-kit)

(defun parser-resource-error (name limit actual span)
  (%signal-yaml-resource-limit name limit actual :span span :context "YAML"))

(defun append-tag-directive (parser handle prefix allow-duplicates span)
  (when (and (not allow-duplicates)
             (assoc handle (parser-directives parser) :test #'string=))
    (parser-error (make-token :tag-directive span span) "found duplicate %TAG directive"))
  (push (cons handle prefix) (parser-directives parser)))

(defun process-directives (parser)
  (setf (parser-directives parser) nil
        (parser-version parser) nil)
  (let ((saw-directive nil))
    (loop for token = (parser-peek parser)
          while (member (token-kind token) '(:version-directive :tag-directive))
          do (setf saw-directive t)
             (parser-next parser)
             (case (token-kind token)
               (:version-directive
                (when (parser-version parser)
                  (parser-error token "found duplicate %YAML directive"))
                (setf (parser-version parser)
                      (cons (token-major token) (token-minor token))))
               (:tag-directive
                (append-tag-directive parser (token-handle token)
                                      (token-value token) nil
                                      (token-start-span token)))))
    (let ((span (token-start-span (parser-peek parser))))
      (dolist (directive *default-tag-directives*)
        (append-tag-directive parser (car directive) (cdr directive) t span))
      saw-directive)))

(defun parser-tag-token (parser token)
  (let ((handle (token-handle token)) (suffix (token-suffix token)))
    (if (or (null handle) (zerop (length handle)))
        suffix
        (let ((prefix (cdr (find handle (parser-directives parser)
                                 :key #'car :test #'string= :from-end t))))
          (unless prefix (parser-error token "found undefined tag handle"))
          (concatenate 'simple-string prefix suffix)))))

(declaim (inline parser-implicit-tag-p))
(defun parser-implicit-tag-p (tag)
  (or (null tag) (zerop (length tag))))

(defun parser-empty-scalar (parser start-span &optional end-span anchor tag)
  (let ((end-span (or end-span start-span)))
    (parser-emit parser (make-scalar-event :start-span start-span :end-span end-span
                                           :anchor anchor :tag tag :value ""
                                           :style :plain
                                           :plain-implicit-p (parser-implicit-tag-p tag)))))

(define-parser-state yaml-parser-parse-stream-start (parser)
  (let ((token (parser-next parser)))
    (parser-emit parser (make-stream-start-event :start-span (token-start-span token)
                                                 :end-span (token-end-span token))))
  #'yaml-parser-parse-document-start)

(define-parser-state yaml-parser-parse-document-start (parser)
  (let ((saw-directive (process-directives parser)))
    (let ((token (parser-peek parser)) (explicit nil))
    (when (and saw-directive (not (eq (token-kind token) :document-start)))
      (parser-error token "did not find expected document start"))
    (when (eq (token-kind token) :stream-end)
      (parser-emit parser (make-stream-end-event :start-span (token-start-span token)
                                                 :end-span (token-end-span token)))
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
    (parser-emit parser (make-document-start-event :start-span (token-start-span token)
                         :end-span (token-end-span token) :explicit-p explicit
                         :version (parser-version parser)
                         :tag-directives (copy-list (parser-directives parser))))))
  #'yaml-parser-parse-document-content)

(define-parser-state yaml-parser-parse-document-content (parser)
  (let ((token (parser-peek parser)))
    (if (member (token-kind token)
                '(:document-start :document-end :stream-end :block-end))
        (parser-empty-scalar-state parser (token-start-span token)
                                   #'yaml-parser-parse-document-end)
        (parser-push-node-state parser
                                (#'yaml-parser-parse-document-end)
                                #'yaml-parser-parse-node-block))))

(define-parser-state yaml-parser-parse-document-end (parser)
  (let ((token (parser-peek parser)) (explicit nil))
    (when (eq (token-kind token) :document-end) (setf explicit t) (parser-next parser))
    (parser-emit parser (make-document-end-event :start-span (token-start-span token)
                         :end-span (token-end-span token) :explicit-p explicit))
    (setf (parser-directives parser) nil (parser-version parser) nil)
    (let ((next (parser-peek parser)))
      (when (and explicit next
                 (= (cl-parser-kit:span-start-line (token-end-span token))
                    (cl-parser-kit:span-start-line (token-start-span next)))
                 (not (member (token-kind next)
                              '(:stream-end :document-start
                                :version-directive :tag-directive))))
        (parser-error next "did not find expected document start"))
      (when (and (not explicit) next
                 (member (token-kind next) '(:version-directive :tag-directive)))
        (parser-error next "did not find expected document end"))
      (when (and (not explicit) next
                 (= (cl-parser-kit:span-start-line (token-end-span token))
                    (cl-parser-kit:span-start-line (token-start-span next)))
                 (not (member (token-kind next)
                              '(:stream-end :document-start :version-directive
                                :tag-directive))))
        (parser-error next "did not find expected document start"))
      (if (eq (token-kind next) :stream-end)
          #'yaml-parser-parse-stream-end #'yaml-parser-parse-document-start))))

(define-parser-state yaml-parser-parse-stream-end (parser)
  (let ((token (parser-next parser)))
    (parser-emit parser (make-stream-end-event :start-span (token-start-span token)
                                               :end-span (token-end-span token))))
  nil)
