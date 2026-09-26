(in-package #:yaml-kit)

(defun parser-resource-error (parser name limit actual mark)
  (declare (ignore parser))
  (error 'yaml-resource-limit-error :limit-name name :limit limit :actual actual :mark mark))

(defun append-tag-directive (parser handle prefix allow-duplicates mark)
  "yaml_parser_append_tag_directive."
  (when (and (not allow-duplicates)
             (assoc handle (parser-directives parser) :test #'string=))
    (parser-error (make-token :tag-directive mark mark) "found duplicate %TAG directive"))
  (push (cons handle prefix) (parser-directives parser)))

(defun process-directives (parser)
  "yaml_parser_process_directives."
  (setf (parser-directives parser) nil (parser-version parser) nil)
  (loop for token = (parser-peek parser)
        while (member (token-kind token) '(:version-directive :tag-directive)) do
    (parser-next parser)
    (case (token-kind token)
      (:version-directive
       (when (parser-version parser) (parser-error token "found duplicate %YAML directive"))
       (unless (and (= (token-major token) 1) (member (token-minor token) '(1 2)))
         (parser-error token "found incompatible YAML document"))
       (setf (parser-version parser) (cons (token-major token) (token-minor token))))
      (:tag-directive
       (append-tag-directive parser (token-handle token) (token-value token)
                             nil (token-start-mark token)))))
  (let ((mark (token-start-mark (parser-peek parser))))
    (append-tag-directive parser "!" "!" t mark)
    (append-tag-directive parser "!!" "tag:yaml.org,2002:" t mark)))

(defun parser-tag-token (parser token)
  (let ((handle (token-handle token)) (suffix (token-suffix token)))
    (cond ((or (null handle) (zerop (length handle))) suffix)
          ((and (string= handle "!") (> (length suffix) 1)
                (char= (char suffix 0) #\<)
                (char= (char suffix (1- (length suffix))) #\>))
           (subseq suffix 1 (1- (length suffix))))
          (t (let ((prefix (cdr (assoc handle (parser-directives parser) :test #'string=))))
               (unless prefix (parser-error token "found undefined tag handle"))
               (concatenate 'simple-string prefix suffix))))))

(declaim (inline parser-implicit-tag-p))
(defun parser-implicit-tag-p (tag)
  (or (null tag) (zerop (length tag))))

(defun parser-empty-scalar (parser mark)
  (parser-emit parser (make-scalar-event :start-mark mark :end-mark mark :value ""
                                         :style :plain :plain-implicit-p t)))

(define-parser-state yaml-parser-parse-stream-start (parser)
  "yaml_parser_parse_stream_start."
  (let ((token (parser-next parser)))
    (unless (and token (eq (token-kind token) :stream-start))
      (parser-error token "expected stream-start token"))
    (parser-emit parser (make-stream-start-event :start-mark (token-start-mark token)
                                                 :end-mark (token-end-mark token))))
  #'yaml-parser-parse-document-start)

(define-parser-state yaml-parser-parse-document-start (parser)
  "yaml_parser_parse_document_start."
  (process-directives parser)
  (let ((token (parser-peek parser)) (explicit nil))
    (when (eq (token-kind token) :stream-end)
      (parser-emit parser (make-stream-end-event :start-mark (token-start-mark token)
                                                 :end-mark (token-end-mark token)))
      (parser-next parser)
      (return-from yaml-parser-parse-document-start nil))
    (when (eq (token-kind token) :document-start)
      (setf explicit t) (parser-next parser))
    (parser-emit parser (make-document-start-event :start-mark (token-start-mark token)
                         :end-mark (token-end-mark token) :explicit-p explicit
                         :version (parser-version parser)
                         :tag-directives (copy-list (parser-directives parser)))))
  #'yaml-parser-parse-document-content)

(define-parser-state yaml-parser-parse-document-content (parser)
  "yaml_parser_parse_document_content."
  (let ((token (parser-peek parser)))
    (if (member (token-kind token) '(:document-end :stream-end :block-end))
        (progn (parser-empty-scalar parser (token-start-mark token)) #'yaml-parser-parse-document-end)
        (progn (parser-push parser #'yaml-parser-parse-document-end) #'yaml-parser-parse-node-block))))

(define-parser-state yaml-parser-parse-document-end (parser)
  "yaml_parser_parse_document_end."
  (let ((token (parser-peek parser)) (explicit nil))
    (when (eq (token-kind token) :document-end) (setf explicit t) (parser-next parser))
    (parser-emit parser (make-document-end-event :start-mark (token-start-mark token)
                         :end-mark (token-end-mark token) :explicit-p explicit)))
  (setf (parser-directives parser) nil (parser-version parser) nil)
  (if (eq (token-kind (parser-peek parser)) :stream-end)
      #'yaml-parser-parse-stream-end #'yaml-parser-parse-document-start))

(define-parser-state yaml-parser-parse-stream-end (parser)
  "yaml_parser_parse_stream_end."
  (let ((token (parser-next parser)))
    (unless (eq (token-kind token) :stream-end) (parser-error token "expected stream-end token"))
    (parser-emit parser (make-stream-end-event :start-mark (token-start-mark token)
                                               :end-mark (token-end-mark token))))
  nil)

(defun parser-node (parser block indentless)
  "yaml_parser_parse_node."
  (let ((token (parser-peek parser)) (anchor nil) (tag nil) (start nil))
    (unless token (parser-error token "expected YAML node"))
    (if (eq (token-kind token) :alias)
        (progn (parser-next parser)
               (parser-emit parser (make-alias-event :start-mark (token-start-mark token)
                                                     :end-mark (token-end-mark token)
                                                     :anchor (token-value token)))
               (parser-pop parser))
        (progn
          (setf start (token-start-mark token))
          (when (eq (token-kind token) :anchor) (setf anchor (token-value token)) (parser-next parser) (setf token (parser-peek parser)))
          (when (eq (token-kind token) :tag) (setf tag (parser-tag-token parser token)) (parser-next parser) (setf token (parser-peek parser)))
          (when (eq (token-kind token) :anchor) (setf anchor (token-value token)) (parser-next parser) (setf token (parser-peek parser)))
          (if (and indentless (eq (token-kind token) :block-entry))
              (progn
                (incf (parser-depth parser))
                (when (> (parser-depth parser) (parser-max-depth parser))
                  (parser-resource-error parser "depth" (parser-max-depth parser) (parser-depth parser) start))
                (parser-emit parser (make-sequence-start-event :start-mark start :end-mark (token-end-mark token)
                                                               :anchor anchor :tag tag :implicit-p (parser-implicit-tag-p tag) :style :block))
                #'yaml-parser-parse-indentless-sequence-entry)
            (case (token-kind token)
            (:scalar
             (parser-next parser)
             (when (> (length (token-value token)) (parser-max-scalar-length parser))
               (parser-resource-error parser "scalar" (parser-max-scalar-length parser)
                                      (length (token-value token)) (token-start-mark token)))
             (parser-emit parser (make-scalar-event :start-mark start :end-mark (token-end-mark token)
                                                    :anchor anchor :tag tag :value (token-value token)
                                                    :style (token-style token)
                                                    :plain-implicit-p (or (and (null tag) (eq (token-style token) :plain)) (string= (or tag "") "!"))
                                                    :quoted-implicit-p (and (null tag) (not (eq (token-style token) :plain)))))
             (parser-pop parser))
            (:flow-sequence-start
             (parser-next parser)
             (incf (parser-depth parser))
             (when (> (parser-depth parser) (parser-max-depth parser))
               (parser-resource-error parser "depth" (parser-max-depth parser) (parser-depth parser) start))
             (parser-emit parser (make-sequence-start-event :start-mark start :end-mark (token-end-mark token)
                                                             :anchor anchor :tag tag :implicit-p (parser-implicit-tag-p tag) :style :flow))
             #'yaml-parser-parse-flow-sequence-entry)
            (:flow-mapping-start
             (parser-next parser)
             (incf (parser-depth parser))
             (when (> (parser-depth parser) (parser-max-depth parser))
               (parser-resource-error parser "depth" (parser-max-depth parser) (parser-depth parser) start))
             (parser-emit parser (make-mapping-start-event :start-mark start :end-mark (token-end-mark token)
                                                           :anchor anchor :tag tag :implicit-p (parser-implicit-tag-p tag) :style :flow))
             #'yaml-parser-parse-flow-mapping-key)
            (:block-sequence-start
             (unless block (parser-error token "did not find expected node content"))
             (parser-next parser)
             (incf (parser-depth parser))
             (when (> (parser-depth parser) (parser-max-depth parser))
               (parser-resource-error parser "depth" (parser-max-depth parser) (parser-depth parser) start))
             (parser-emit parser (make-sequence-start-event :start-mark start :end-mark (token-end-mark token)
                                                             :anchor anchor :tag tag :implicit-p (parser-implicit-tag-p tag) :style :block))
             #'yaml-parser-parse-block-sequence-entry)
            (:block-mapping-start
             (unless block (parser-error token "did not find expected node content"))
             (parser-next parser)
             (incf (parser-depth parser))
             (when (> (parser-depth parser) (parser-max-depth parser))
               (parser-resource-error parser "depth" (parser-max-depth parser) (parser-depth parser) start))
             (parser-emit parser (make-mapping-start-event :start-mark start :end-mark (token-end-mark token)
                                                           :anchor anchor :tag tag :implicit-p (parser-implicit-tag-p tag) :style :block))
             #'yaml-parser-parse-block-mapping-key)
             (otherwise
              (if (or anchor tag) (progn (parser-empty-scalar parser start) (parser-pop parser))
                  (parser-error token "did not find expected node content")))))))))

(define-parser-state yaml-parser-parse-node-block (parser) "yaml_parser_parse_node."
  (parser-node parser t nil))
(define-parser-state yaml-parser-parse-node-block-indentless (parser) "yaml_parser_parse_node."
  (parser-node parser t t))
(define-parser-state yaml-parser-parse-node-flow (parser) "yaml_parser_parse_node."
  (parser-node parser nil nil))

(define-parser-state yaml-parser-parse-block-sequence-entry (parser)
  "yaml_parser_parse_block_sequence_entry."
  (let ((token (parser-peek parser)))
    (if (eq (token-kind token) :block-end)
        (progn (parser-next parser) (parser-emit parser (make-sequence-end-event :start-mark (token-start-mark token) :end-mark (token-end-mark token))) (decf (parser-depth parser)) (parser-pop parser))
        (progn (when (eq (token-kind token) :block-entry) (parser-next parser))
               (parser-push parser #'yaml-parser-parse-block-sequence-entry)
               (if (member (token-kind (parser-peek parser)) '(:block-entry :block-end))
                   (progn (parser-empty-scalar parser (token-start-mark (parser-peek parser))) #'yaml-parser-parse-block-sequence-entry)
                   #'yaml-parser-parse-node-block)))))

(define-parser-state yaml-parser-parse-indentless-sequence-entry (parser)
  "yaml_parser_parse_indentless_sequence_entry."
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
        (progn (parser-emit parser (make-sequence-end-event :start-mark (token-start-mark token) :end-mark (token-end-mark token)))
               (decf (parser-depth parser)) (parser-pop parser)))))

(define-parser-state yaml-parser-parse-block-mapping-key (parser)
  "yaml_parser_parse_block_mapping_key."
  (let ((token (parser-peek parser)))
    (if (eq (token-kind token) :block-end)
        (progn (parser-next parser)
               (parser-emit parser (make-mapping-end-event :start-mark (token-start-mark token) :end-mark (token-end-mark token)))
               (decf (parser-depth parser)) (parser-pop parser))
        (progn (when (eq (token-kind token) :key) (parser-next parser))
               (parser-push parser #'yaml-parser-parse-block-mapping-value)
               (if (member (token-kind (parser-peek parser)) '(:value :block-end))
                   (progn (parser-empty-scalar parser (token-start-mark (parser-peek parser))) #'yaml-parser-parse-block-mapping-value)
                   #'yaml-parser-parse-node-block-indentless)))))

(define-parser-state yaml-parser-parse-block-mapping-value (parser)
  "yaml_parser_parse_block_mapping_value."
  (let ((token (parser-peek parser)))
    (when (eq (token-kind token) :value) (parser-next parser))
    (if (member (token-kind (parser-peek parser)) '(:key :value :block-end))
        (progn (parser-empty-scalar parser (token-start-mark (parser-peek parser))) #'yaml-parser-parse-block-mapping-key)
        (progn (parser-push parser #'yaml-parser-parse-block-mapping-key)
               #'yaml-parser-parse-node-block-indentless))))

(define-parser-state yaml-parser-parse-flow-sequence-entry (parser)
  "yaml_parser_parse_flow_sequence_entry."
  (let ((token (parser-peek parser)))
    (cond ((eq (token-kind token) :flow-sequence-end) (parser-next parser) (parser-emit parser (make-sequence-end-event :start-mark (token-start-mark token) :end-mark (token-end-mark token))) (decf (parser-depth parser)) (parser-pop parser))
          ((eq (token-kind token) :flow-entry) (parser-next parser) #'yaml-parser-parse-flow-sequence-entry)
          ((eq (token-kind token) :key)
           (parser-next parser)
           (parser-emit parser (make-mapping-start-event :start-mark (token-start-mark token) :end-mark (token-end-mark token) :implicit-p t :style :flow))
           (parser-push parser #'yaml-parser-parse-flow-sequence-entry)
           (parser-push parser #'yaml-parser-parse-flow-sequence-entry-mapping-end)
           (parser-push parser #'yaml-parser-parse-flow-sequence-entry-mapping-value)
           #'yaml-parser-parse-node-flow)
          (t (parser-push parser #'yaml-parser-parse-flow-sequence-entry) #'yaml-parser-parse-node-flow))))

(define-parser-state yaml-parser-parse-flow-sequence-entry-mapping-value (parser)
  "yaml_parser_parse_flow_sequence_entry_mapping_value."
  (let ((token (parser-peek parser)))
    (when (eq (token-kind token) :value) (parser-next parser))
    (if (member (token-kind (parser-peek parser)) '(:flow-entry :flow-sequence-end))
        (progn (parser-empty-scalar parser (token-start-mark (parser-peek parser)))
               (parser-pop parser))
        (progn (parser-push parser #'yaml-parser-parse-flow-sequence-entry-mapping-end)
               #'yaml-parser-parse-node-flow))))

(define-parser-state yaml-parser-parse-flow-sequence-entry-mapping-end (parser)
  "yaml_parser_parse_flow_sequence_entry_mapping_end."
  (let ((token (parser-peek parser)))
    (parser-emit parser (make-mapping-end-event :start-mark (token-start-mark token)
                                                :end-mark (token-end-mark token))))
  (parser-pop parser))

(define-parser-state yaml-parser-parse-flow-mapping-key (parser)
  "yaml_parser_parse_flow_mapping_key."
  (let ((token (parser-peek parser)))
    (cond ((eq (token-kind token) :flow-mapping-end) (parser-next parser) (parser-emit parser (make-mapping-end-event :start-mark (token-start-mark token) :end-mark (token-end-mark token))) (decf (parser-depth parser)) (parser-pop parser))
          ((eq (token-kind token) :flow-entry) (parser-next parser) #'yaml-parser-parse-flow-mapping-key)
          ((eq (token-kind token) :key) (parser-next parser) (parser-push parser #'yaml-parser-parse-flow-mapping-value) (if (eq (token-kind (parser-peek parser)) :value) (progn (parser-empty-scalar parser (token-start-mark (parser-peek parser))) #'yaml-parser-parse-flow-mapping-value) #'yaml-parser-parse-node-flow))
          (t (parser-push parser #'yaml-parser-parse-flow-mapping-value) #'yaml-parser-parse-node-flow))))

(define-parser-state yaml-parser-parse-flow-mapping-value (parser)
  "yaml_parser_parse_flow_mapping_value."
  (let ((token (parser-peek parser)))
    (when (eq (token-kind token) :value) (parser-next parser))
    (if (member (token-kind (parser-peek parser)) '(:flow-entry :flow-mapping-end))
        (progn (parser-empty-scalar parser (token-start-mark (parser-peek parser))) #'yaml-parser-parse-flow-mapping-key)
        (progn (parser-push parser #'yaml-parser-parse-flow-mapping-key)
               #'yaml-parser-parse-node-flow))))

(defun yaml-parser-parse (parser)
  "yaml_parser_parse."
  (setf (parser-state parser) (funcall (parser-state parser) parser)))

(defun yaml-parser-state-machine (parser)
  "yaml_parser_state_machine."
  (yaml-parser-parse parser))

(defun yaml-parser-parse-node (parser block indentless)
  "yaml_parser_parse_node."
  (parser-node parser block indentless))

(defun yaml-parser-parse-flow-sequence-entry-mapping-key (parser)
  "yaml_parser_parse_flow_sequence_entry_mapping_key."
  (yaml-parser-parse-flow-sequence-entry parser))

(defun %parser-octet-encoding (input)
  (let ((n (length input)))
    (cond ((and (>= n 4) (= (aref input 0) 0) (= (aref input 1) 0) (= (aref input 2) #xfe) (= (aref input 3) #xff)) (values :utf-32be 4))
          ((and (>= n 4) (= (aref input 0) #xff) (= (aref input 1) #xfe) (= (aref input 2) 0) (= (aref input 3) 0)) (values :utf-32le 4))
          ((and (>= n 2) (= (aref input 0) #xfe) (= (aref input 1) #xff)) (values :utf-16be 2))
          ((and (>= n 2) (= (aref input 0) #xff) (= (aref input 1) #xfe)) (values :utf-16le 2))
          ((and (>= n 3) (= (aref input 0) #xef) (= (aref input 1) #xbb) (= (aref input 2) #xbf)) (values :utf-8 3))
          (t (values :utf-8 0)))))

(defun %parser-input-string (input)
  (typecase input
    (string (coerce input 'simple-string))
    (stream (with-output-to-string (out) (loop for c = (read-char input nil nil) while c do (write-char c out))))
    ((vector (unsigned-byte 8))
     (multiple-value-bind (encoding start) (%parser-octet-encoding input)
       (coerce (cl-codec-kit:octets-to-string input :start start :encoding encoding :errorp t) 'simple-string)))
    (otherwise (error 'yaml-parse-error :context "invalid YAML input"))))

(defun map-events (handler input &key (max-input-length 104857600) (max-depth 256) (max-scalar-length 16777216))
  (let ((text (%parser-input-string input)))
    (when (> (length text) max-input-length) (error 'yaml-resource-limit-error :limit-name "input length" :limit max-input-length :actual (length text)))
    (let ((parser (make-parser% :scanner (make-scanner text)
                                :state #'yaml-parser-parse-stream-start :states nil :marks nil
                                :handler handler :directives nil :version nil :depth 0
                                :max-depth max-depth :max-scalar-length max-scalar-length)))
      (loop for state = (parser-state parser) while state do (setf (parser-state parser) (funcall state parser)))))
  nil)

(defun parse-events (input &rest keys)
  (let ((events nil)) (apply #'map-events (lambda (event) (push event events)) input keys) (nreverse events)))
