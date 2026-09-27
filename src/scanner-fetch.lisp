;;;; src/scanner-fetch.lisp
(in-package #:yaml-kit)

(declaim (optimize (speed 3) (safety 1)))

(defun fetch-stream-start (s)
  "yaml_parser_fetch_stream_start."
  (setf (scanner-indent s) -1
        (scanner-simple-keys s) (list (make-simple-key))
        (scanner-simple-key-allowed s) t
        (scanner-stream-start-produced s) t)
  (let ((mark (sc-mark s)))
    (enqueue-token s (make-token :stream-start mark mark))))

(defun fetch-stream-end (s)
  "yaml_parser_fetch_stream_end."
  (unless (zerop (scanner-column s))
    (setf (scanner-column s) 0)
    (incf (scanner-line s)))
  (unroll-indent s -1)
  (remove-simple-key s)
  (setf (scanner-simple-key-allowed s) nil)
  (let ((mark (sc-mark s)))
    (enqueue-token s (make-token :stream-end mark mark))))

(defun fetch-directive (s)
  "yaml_parser_fetch_directive."
  (unroll-indent s -1)
  (remove-simple-key s)
  (setf (scanner-simple-key-allowed s) nil)
  (enqueue-token s (scan-directive s)))

(defun fetch-document-indicator (s kind)
  "yaml_parser_fetch_document_indicator."
  (unroll-indent s -1)
  (remove-simple-key s)
  (setf (scanner-simple-key-allowed s) nil)
  (let ((start (sc-mark s)))
    (dotimes (i 3) (sc-skip s))
    (enqueue-token s (make-token kind start (sc-mark s)))))

(defun fetch-flow-collection-start (s kind)
  "yaml_parser_fetch_flow_collection_start."
  (save-simple-key s)
  (increase-flow-level s)
  (setf (scanner-simple-key-allowed s) t)
  (let ((start (sc-mark s)))
    (sc-skip s)
    (enqueue-token s (make-token kind start (sc-mark s)))))

(defun fetch-flow-collection-end (s kind)
  "yaml_parser_fetch_flow_collection_end."
  (when (and (eq kind :flow-sequence-end)
             (sc-check s #\# 1))
    (sc-error s "while scanning a flow collection" (sc-mark s)
              "did not find expected separation space"))
  (remove-simple-key s)
  (decrease-flow-level s)
  (setf (scanner-simple-key-allowed s) nil)
  (let ((start (sc-mark s)))
    (sc-skip s)
    (enqueue-token s (make-token kind start (sc-mark s)))))

(defun fetch-flow-entry (s)
  "yaml_parser_fetch_flow_entry."
  ;; YAML 1.2.2 lists "," among the c-flow-indicator characters, so it can only
  ;; separate entries inside a flow collection.
  (unless (plusp (scanner-flow-level s))
    (sc-error s "while scanning for the next token" (sc-mark s)
              "found character that cannot start any token"))
  (when (sc-check s #\# 1)
    (sc-error s "while scanning a flow collection" (sc-mark s)
              "did not find expected separation space"))
  (when (plusp (scanner-flow-level s))
    (let ((i (1- (scanner-pos s))))
      (loop while (and (>= i 0) (member (char (scanner-text s) i) '(#\Space #\Tab)))
            do (decf i))
      (when (and (>= i 0) (char= (char (scanner-text s) i) #\[))
        (sc-error s "while scanning a flow collection" (sc-mark s)
                  "did not find expected node content"))))
  (remove-simple-key s)
  (setf (scanner-simple-key-allowed s) t)
  (let ((start (sc-mark s)))
    (sc-skip s)
    (enqueue-token s (make-token :flow-entry start (sc-mark s)))))

(defun fetch-block-entry (s)
  "yaml_parser_fetch_block_entry."
  (unless (plusp (scanner-flow-level s))
    (unless (scanner-simple-key-allowed s)
      (sc-error s nil (sc-mark s)
                "block sequence entries are not allowed in this context"))
    (roll-indent s (scanner-column s) -1 :block-sequence-start (sc-mark s)))
  ;; YAML 1.2.2 leaves a '-' in flow context for the parser to diagnose,
  ;; matching libyaml's scanner/parser boundary.
  (remove-simple-key s)
  (setf (scanner-simple-key-allowed s) t)
  (let ((start (sc-mark s)))
    (sc-skip s)
    (enqueue-token s (make-token :block-entry start (sc-mark s)))))

(defun fetch-key (s)
  "yaml_parser_fetch_key."
  (unless (plusp (scanner-flow-level s))
    (unless (scanner-simple-key-allowed s)
      (sc-error s nil (sc-mark s) "mapping keys are not allowed in this context"))
    (roll-indent s (scanner-column s) -1 :block-mapping-start (sc-mark s)))
  (remove-simple-key s)
  (setf (scanner-simple-key-allowed s) (zerop (scanner-flow-level s)))
  (let ((start (sc-mark s)))
    (sc-skip s)
    (enqueue-token s (make-token :key start (sc-mark s)))))

(defun fetch-value (s)
  "yaml_parser_fetch_value."
  (let ((simple-key (car (scanner-simple-keys s))))
    (if (simple-key-possible simple-key)
        (progn
          (insert-token s (- (simple-key-token-number simple-key)
                             (scanner-tokens-parsed s))
                        (make-token :key (simple-key-mark simple-key)
                                    (simple-key-mark simple-key)))
          (roll-indent s (mark-column (simple-key-mark simple-key))
                       (simple-key-token-number simple-key)
                       :block-mapping-start (simple-key-mark simple-key))
          (setf (simple-key-possible simple-key) nil
                (scanner-simple-key-allowed s) nil))
        (progn
          (unless (plusp (scanner-flow-level s))
            (unless (scanner-simple-key-allowed s)
              (sc-error s nil (sc-mark s)
                        "mapping values are not allowed in this context"))
            (roll-indent s (scanner-column s) -1 :block-mapping-start
                         (sc-mark s)))
          (setf (scanner-simple-key-allowed s)
                (zerop (scanner-flow-level s)))))
    (let ((start (sc-mark s)))
      (sc-skip s)
      (enqueue-token s (make-token :value start (sc-mark s))))))

(defun fetch-anchor (s kind)
  "yaml_parser_fetch_anchor."
  (save-simple-key s)
  (setf (scanner-simple-key-allowed s) nil)
  (enqueue-token s (scan-anchor s kind)))

(defun fetch-tag (s)
  "yaml_parser_fetch_tag."
  (save-simple-key s)
  (setf (scanner-simple-key-allowed s) nil)
  (enqueue-token s (scan-tag s)))

(defun fetch-block-scalar (s literal-p)
  "yaml_parser_fetch_block_scalar."
  (remove-simple-key s)
  (setf (scanner-simple-key-allowed s) t)
  (enqueue-token s (scan-block-scalar s literal-p)))

(defun fetch-flow-scalar (s single-p)
  "yaml_parser_fetch_flow_scalar."
  (save-simple-key s)
  (setf (scanner-simple-key-allowed s) nil)
  (enqueue-token s (scan-flow-scalar s single-p)))

(defun fetch-plain-scalar (s)
  "yaml_parser_fetch_plain_scalar."
  (save-simple-key s)
  (setf (scanner-simple-key-allowed s) nil)
  (enqueue-token s (scan-plain-scalar s)))
