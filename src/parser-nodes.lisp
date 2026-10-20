(in-package #:yaml-kit)

(defun parser-increment-depth (parser span)
  (incf (parser-depth parser))
  (when (> (parser-depth parser) (parser-max-depth parser))
    (parser-resource-error "depth" (parser-max-depth parser) (parser-depth parser) span)))

(defun parser-node-properties (parser token)
  (let ((anchor nil) (tag nil) (start (token-start-span token)) (end (token-start-span token)))
    (loop while (member (token-kind token) '(:anchor :tag)) do
      (if (eq (token-kind token) :tag)
          (progn (when tag (parser-error token "did not find expected node content"))
                 (setf tag (parser-tag-token parser token)))
          (progn (when anchor (parser-error token "did not find expected node content"))
                 (setf anchor (token-value token))))
      (setf end (token-end-span token))
      (parser-next parser)
      (setf token (parser-peek parser)))
    (when (and (or anchor tag) (member (token-kind token) '(:anchor :tag :alias)))
      (parser-error token "did not find expected node content"))
    (values token anchor tag start end)))

(defun parser-start-collection (parser token start anchor tag)
  (let ((kind (token-kind token)))
    (parser-next parser)
    (parser-increment-depth parser start)
    (case kind
      ((:flow-sequence-start :block-sequence-start)
       (parser-emit parser (make-sequence-start-event
                            :start-span start :end-span (token-end-span token)
                            :anchor anchor :tag tag
                            :implicit-p (parser-implicit-tag-p tag)
                            :style (if (eq kind :flow-sequence-start) :flow :block)))
       (if (eq kind :flow-sequence-start)
           #'yaml-parser-parse-flow-sequence-first-entry
           #'yaml-parser-parse-block-sequence-entry))
      ((:flow-mapping-start :block-mapping-start)
       (parser-emit parser (make-mapping-start-event
                            :start-span start :end-span (token-end-span token)
                            :anchor anchor :tag tag
                            :implicit-p (parser-implicit-tag-p tag)
                            :style (if (eq kind :flow-mapping-start) :flow :block)))
       (if (eq kind :flow-mapping-start)
           #'yaml-parser-parse-flow-mapping-first-key
           #'yaml-parser-parse-block-mapping-key)))))

(defun parser-node-content (parser token start end anchor tag)
  (case (token-kind token)
    (:scalar
     (parser-next parser)
     (when (> (length (token-value token)) (parser-max-scalar-length parser))
       (parser-resource-error "scalar" (parser-max-scalar-length parser)
                              (length (token-value token))
                              (token-start-span token)))
     (parser-emit parser (make-scalar-event
                          :start-span start :end-span (token-end-span token)
                          :anchor anchor :tag tag :value (token-value token)
                          :style (token-style token)
                          :plain-implicit-p (or (and (null tag) (eq (token-style token) :plain))
                                                (string= (or tag "") "!"))
                          :quoted-implicit-p
                          (and (null tag) (not (eq (token-style token) :plain)))))
     (parser-pop parser))
    ((:flow-sequence-start :flow-mapping-start :block-sequence-start :block-mapping-start)
     (parser-start-collection parser token start anchor tag))
    (otherwise
     (if (or anchor tag)
         (progn (parser-empty-scalar parser start end anchor tag) (parser-pop parser))
       (parser-error token "did not find expected node content")))))

(defun parser-node (parser indentless)
  (let ((token (parser-peek parser)))
    (if (eq (token-kind token) :alias)
        (progn (parser-next parser)
               (parser-emit parser (make-alias-event :start-span (token-start-span token)
                                                     :end-span (token-end-span token)
                                                     :anchor (token-value token)))
               (parser-pop parser))
        (multiple-value-bind (content anchor tag start end)
            (parser-node-properties parser token)
          (if (and indentless (eq (token-kind content) :block-entry))
              (progn
                (parser-increment-depth parser start)
                (parser-emit parser (make-sequence-start-event
                                     :start-span start :end-span (token-end-span content)
                                     :anchor anchor :tag tag
                                     :implicit-p (parser-implicit-tag-p tag) :style :block))
                #'yaml-parser-parse-indentless-sequence-entry)
              (parser-node-content parser content start end anchor tag))))))

(define-parser-state yaml-parser-parse-node-block (parser)
  (parser-node parser nil))
(define-parser-state yaml-parser-parse-node-block-indentless (parser)
  (parser-node parser t))
(define-parser-state yaml-parser-parse-node-flow (parser)
  (parser-node parser nil))

(define-parser-state yaml-parser-parse-block-sequence-entry (parser)
  (let ((token (parser-peek parser)))
    (if (eq (token-kind token) :block-end)
        (parser-close-collection parser token #'make-sequence-end-event
                                 (token-end-span token))
        (progn
          (when (and (eq (token-kind token) :block-sequence-start)
                     (eq (scanner-previous-token-kind (parser-scanner parser))
                         :block-end))
            (parser-error token "did not find expected node content"))
          (when (eq (token-kind token) :block-entry)
            (parser-next parser))
          ;; Only the branch that parses a node may leave a continuation for
          ;; that node to pop; an empty entry has already been consumed.
          (if (member (token-kind (parser-peek parser)) '(:block-entry :block-end))
              (parser-empty-scalar-state parser
                                         (token-start-span (parser-peek parser))
                                         #'yaml-parser-parse-block-sequence-entry)
              (parser-push-node-state parser
                                      (#'yaml-parser-parse-block-sequence-entry)
                                      #'yaml-parser-parse-node-block))))))

(define-parser-state yaml-parser-parse-indentless-sequence-entry (parser)
  (let ((token (parser-peek parser)))
    (if (eq (token-kind token) :block-entry)
        (let ((span (token-end-span token)))
          (parser-next parser)
          (if (member (token-kind (parser-peek parser))
                      '(:block-entry :key :value :block-end))
              (parser-empty-scalar-state
               parser span #'yaml-parser-parse-indentless-sequence-entry)
              (parser-push-node-state parser
                                      (#'yaml-parser-parse-indentless-sequence-entry)
                                      #'yaml-parser-parse-node-block)))
        (parser-close-collection parser token #'make-sequence-end-event
                                 (token-start-span token) nil))))

(define-parser-state yaml-parser-parse-block-mapping-key (parser)
  (let ((token (parser-peek parser)))
    (cond
      ((eq (token-kind token) :block-entry)
       (parser-error token "did not find expected mapping key"))
      ((eq (token-kind token) :block-end)
       (parser-close-collection parser token #'make-mapping-end-event
                                (token-end-span token)))
      ((eq (token-kind token) :value)
       (parser-empty-scalar-state parser (token-start-span token)
                                  #'yaml-parser-parse-block-mapping-value))
      ((eq (token-kind token) :key)
       (let ((next-token (progn (parser-next parser) (parser-peek parser))))
         (if (member (token-kind next-token) '(:value :block-end))
             (parser-empty-scalar-state parser (token-start-span next-token)
                                        #'yaml-parser-parse-block-mapping-value)
             (parser-push-node-state parser
                                     (#'yaml-parser-parse-block-mapping-value)
                                     #'yaml-parser-parse-node-block-indentless))))
      (t
       (parser-error token "did not find expected mapping key")))))

(define-parser-state yaml-parser-parse-block-mapping-value (parser)
  (let ((token (parser-peek parser)))
    (when (eq (token-kind token) :value)
      (parser-next parser))
    (if (member (token-kind (parser-peek parser)) '(:key :value :block-end))
        (parser-empty-scalar-state parser (token-start-span (parser-peek parser))
                                   #'yaml-parser-parse-block-mapping-key)
        (parser-push-node-state parser
                                (#'yaml-parser-parse-block-mapping-key)
                                #'yaml-parser-parse-node-block-indentless))))
