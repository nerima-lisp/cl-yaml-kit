;;;; src/scanner-fetch.lisp
(in-package #:yaml-kit)

(declaim (optimize (speed 3) (safety 1)))

(defun fetch-stream-start (s)
  (setf (scanner-indent s) -1
        (scanner-simple-keys s) (list (make-simple-key))
        (scanner-simple-key-allowed s) t
        (scanner-stream-start-produced s) t)
  (let ((position (sc-position s)))
    (enqueue-token s (make-scanner-token s :stream-start position position))))

(defun fetch-stream-end (s)
  (unless (zerop (scanner-column s))
    (setf (scanner-column s) 0)
    (incf (scanner-line s)))
  (%unroll-indent-and-remove-simple-key s)
  (setf (scanner-simple-key-allowed s) nil)
  (let ((position (sc-position s)))
    (enqueue-token s (make-scanner-token s :stream-end position position))))

(defun fetch-directive (s)
  (%unroll-indent-and-remove-simple-key s)
  (setf (scanner-simple-key-allowed s) nil)
  (enqueue-token s (scan-directive s)))

(defun fetch-document-indicator (s kind)
  (%unroll-indent-and-remove-simple-key s)
  (setf (scanner-simple-key-allowed s) nil)
  (let ((start (sc-position s)))
    (dotimes (i 3) (sc-skip s))
    (enqueue-token s (make-scanner-token s kind start (sc-position s)))))

(defun fetch-flow-collection-start (s kind)
  (save-simple-key s)
  (increase-flow-level s)
  (setf (scanner-simple-key-allowed s) t)
  (%fetch-character-token s kind))

(defun fetch-flow-collection-end (s kind)
  (when (and (eq kind :flow-sequence-end)
             (sc-check s #\# 1))
    (sc-error s "while scanning a flow collection" (sc-position s)
              "did not find expected separation space"))
  (remove-simple-key s)
  (decrease-flow-level s)
  (setf (scanner-simple-key-allowed s) nil)
  (%fetch-character-token s kind))

(defun fetch-flow-entry (s)
  ;; YAML 1.2.2 treats "," as a flow indicator, so it separates entries only
  ;; inside a flow collection.
  (unless (plusp (scanner-flow-level s))
    (sc-error s "while scanning for the next token" (sc-position s)
              "found character that cannot start any token"))
  (when (sc-check s #\# 1)
    (sc-error s "while scanning a flow collection" (sc-position s)
              "did not find expected separation space"))
  ;; Reject an empty first flow-sequence entry when only whitespace separates
  ;; '[' from ','.
  (when (plusp (scanner-flow-level s))
    (let ((i (1- (scanner-pos s))))
      (loop while (and (>= i 0)
                       (yaml-whitespace-p (char (scanner-text s) i)))
            do (decf i))
      (when (and (>= i 0) (char= (char (scanner-text s) i) #\[))
        (sc-error s "while scanning a flow collection" (sc-position s)
                  "did not find expected node content"))))
  (remove-simple-key s)
  (setf (scanner-simple-key-allowed s) t)
  (%fetch-character-token s :flow-entry))

(defun fetch-block-entry (s)
  (unless (plusp (scanner-flow-level s))
    (unless (scanner-simple-key-allowed s)
      (sc-error s nil (sc-position s)
                "block sequence entries are not allowed in this context"))
    (roll-indent s (scanner-column s) -1 :block-sequence-start (sc-position s)))
  ;; YAML 1.2.2 classifies '-' as :block-entry here; flow parser states reject
  ;; that token where a flow node is expected.
  (remove-simple-key s)
  (setf (scanner-simple-key-allowed s) t)
  (%fetch-character-token s :block-entry))

(defun fetch-key (s)
  (unless (plusp (scanner-flow-level s))
    (unless (scanner-simple-key-allowed s)
      (sc-error s nil (sc-position s) "mapping keys are not allowed in this context"))
    (roll-indent s (scanner-column s) -1 :block-mapping-start (sc-position s)))
  (remove-simple-key s)
  (setf (scanner-simple-key-allowed s) (zerop (scanner-flow-level s)))
  (%fetch-character-token s :key))

(defun fetch-value (s)
  (let ((simple-key (car (scanner-simple-keys s))))
    (if (simple-key-possible simple-key)
        (progn
          (let ((position (simple-key-position simple-key))
                (index (- (simple-key-token-number simple-key)
                          (scanner-tokens-parsed s))))
            (if (and (zerop (scanner-flow-level s))
                     (> (sc-position-column position) (scanner-indent s)))
                (roll-indent s (sc-position-column position)
                             (simple-key-token-number simple-key)
                             :block-mapping-start position
                             (list (make-scanner-token s :key position position)))
                (insert-token s index (make-scanner-token s :key position position))))
          (setf (simple-key-possible simple-key) nil
                (scanner-simple-key-allowed s) nil))
        (progn
          (unless (plusp (scanner-flow-level s))
            (unless (scanner-simple-key-allowed s)
              (sc-error s nil (sc-position s)
                        "mapping values are not allowed in this context"))
            (roll-indent s (scanner-column s) -1 :block-mapping-start
                         (sc-position s)))
          (setf (scanner-simple-key-allowed s)
                (zerop (scanner-flow-level s)))))
    (%fetch-character-token s :value)))

(defun fetch-anchor (s kind)
  (%fetch-scanned-token s (scan-anchor s kind)))

(defun fetch-tag (s)
  (%fetch-scanned-token s (scan-tag s)))

(defun fetch-block-scalar (s literal-p)
  (remove-simple-key s)
  (setf (scanner-simple-key-allowed s) t)
  (enqueue-token s (scan-block-scalar s literal-p)))

(defun fetch-flow-scalar (s single-p)
  (%fetch-scanned-token s (scan-flow-scalar s single-p)))

(defun fetch-plain-scalar (s)
  (%fetch-scanned-token s (scan-plain-scalar s)))
