;;;; src/scanner.lisp
(in-package #:yaml-kit)

(declaim (inline scanner-queue-nonempty-p))
(defun scanner-queue-nonempty-p (s)
  (< (scanner-tokens-head s) (fill-pointer (scanner-tokens s))))

(defun scanner-peek-token (s)
  "yaml_parser_scan queue peek."
  (when (scanner-stream-end-produced s) (return-from scanner-peek-token nil))
  (unless (scanner-token-available s) (fetch-more-tokens s))
  (when (scanner-queue-nonempty-p s)
    (aref (scanner-tokens s) (scanner-tokens-head s))))

(defun scanner-next-token (s)
  "yaml_parser_scan."
  (let ((token (scanner-peek-token s)))
    (when token
      (incf (scanner-tokens-head s))
      (incf (scanner-tokens-parsed s))
      (setf (scanner-token-available s) nil)
      (when (eq (token-kind token) :stream-end)
        (setf (scanner-stream-end-produced s) t))
      token)))

(defun fetch-more-tokens (s)
  "yaml_parser_fetch_more_tokens."
  (when (scanner-stream-end-produced s) (return-from fetch-more-tokens nil))
  (loop
    (let ((need-more (not (scanner-queue-nonempty-p s))))
      (unless need-more
        (stale-simple-keys s)
        (dolist (key (scanner-simple-keys s))
          (when (and (simple-key-possible key)
                     (= (simple-key-token-number key)
                        (scanner-tokens-parsed s)))
            (setf need-more t) (return))))
      (unless need-more (return (setf (scanner-token-available s) t)))
      (fetch-next-token s))))

(defun fetch-next-token (s)
  "yaml_parser_fetch_next_token."
  (unless (scanner-stream-start-produced s)
    (return-from fetch-next-token (fetch-stream-start s)))
  (scan-to-next-token s)
  (stale-simple-keys s)
  (unroll-indent s (scanner-column s))
  (let ((c (sc-char s)) (c1 (sc-char s 1)) (c2 (sc-char s 2)))
    (cond
      ((sc-z-p s) (fetch-stream-end s))
      ((and (zerop (scanner-column s)) (char= c #\%)) (fetch-directive s))
      ((and (zerop (scanner-column s)) (char= c #\-) (char= c1 #\-)
            (char= c2 #\-) (sc-blankz-p s 3))
       (fetch-document-indicator s :document-start))
      ((and (zerop (scanner-column s)) (char= c #\.) (char= c1 #\.)
            (char= c2 #\.) (sc-blankz-p s 3))
       (fetch-document-indicator s :document-end))
      ((char= c #\[) (fetch-flow-collection-start s :flow-sequence-start))
      ((char= c #\{) (fetch-flow-collection-start s :flow-mapping-start))
      ((char= c #\]) (fetch-flow-collection-end s :flow-sequence-end))
      ((char= c #\}) (fetch-flow-collection-end s :flow-mapping-end))
      ((char= c #\,) (fetch-flow-entry s))
      ((and (char= c #\-) (sc-blankz-p s 1)) (fetch-block-entry s))
      ((and (char= c #\?)
            (or (sc-blankz-p s 1)
                (and (plusp (scanner-flow-level s))
                     (yaml-flow-indicator-p c1))))
       (fetch-key s))
      ((and (char= c #\:) (or (sc-blankz-p s 1)
                               (and (plusp (scanner-flow-level s))
                                    (or (member c1 '(#\, #\? #\[ #\] #\{ #\}))
                                        (simple-key-possible (car (scanner-simple-keys s)))
                                        (scanner-simple-key-allowed s)))))
       (fetch-value s))
      ((char= c #\*) (fetch-anchor s :alias))
      ((char= c #\&) (fetch-anchor s :anchor))
      ((char= c #\!) (fetch-tag s))
      ((and (char= c #\|) (zerop (scanner-flow-level s))) (fetch-block-scalar s t))
      ((and (char= c #\>) (zerop (scanner-flow-level s))) (fetch-block-scalar s nil))
      ((char= c #\') (fetch-flow-scalar s t))
      ((char= c #\") (fetch-flow-scalar s nil))
      ((or (not (or (sc-blankz-p s) (member c '(#\- #\? #\: #\, #\[ #\]
                                                    #\{ #\} #\# #\& #\* #\!
                                                    #\| #\> #\' #\" #\% #\@ #\`))))
           (and (char= c #\-) (not (sc-blank-p s 1)))
           (and (member c '(#\? #\:))
                (not (sc-blankz-p s 1))))
       (fetch-plain-scalar s))
      (t (sc-error s "while scanning for the next token" (sc-mark s)
                   "found character that cannot start any token")))))

(defun stale-simple-keys (s)
  "yaml_parser_stale_simple_keys."
  (dolist (key (scanner-simple-keys s))
    (when (and (simple-key-possible key)
               (let ((mark (simple-key-mark key)))
                 (or (< (mark-line mark) (scanner-line s))
                     (< (+ (mark-offset mark) 1024) (scanner-pos s)))))
      (when (simple-key-required key)
        (sc-error s "while scanning a simple key" (simple-key-mark key)
                  "could not find expected ':'"))
      (setf (simple-key-possible key) nil)))
  t)

(defun save-simple-key (s)
  "yaml_parser_save_simple_key."
  (when (scanner-simple-key-allowed s)
    (remove-simple-key s)
    (let ((key (first (scanner-simple-keys s))))
      (when key
        (setf (simple-key-possible key) t
              (simple-key-required key)
              (and (zerop (scanner-flow-level s))
                   (= (scanner-indent s) (scanner-column s)))
              (simple-key-token-number key)
              (+ (scanner-tokens-parsed s)
                 (- (fill-pointer (scanner-tokens s)) (scanner-tokens-head s)))
              (simple-key-mark key)
              (sc-mark s)))))
  t)

(defun remove-simple-key (s)
  "yaml_parser_remove_simple_key."
  (let ((key (first (scanner-simple-keys s))))
    (when (and key (simple-key-possible key))
      (when (simple-key-required key)
        (sc-error s "while scanning a simple key" (simple-key-mark key)
                  "could not find expected ':'"))
      (setf (simple-key-possible key) nil)))
  t)

(defun increase-flow-level (s)
  "yaml_parser_increase_flow_level."
  (push (make-simple-key) (scanner-simple-keys s))
  (incf (scanner-flow-level s))
  t)

(defun decrease-flow-level (s)
  "yaml_parser_decrease_flow_level."
  (when (plusp (scanner-flow-level s))
    (decf (scanner-flow-level s))
    (pop (scanner-simple-keys s)))
  t)

(defun roll-indent (s column number kind mark)
  "yaml_parser_roll_indent."
  (when (and (zerop (scanner-flow-level s)) (> column (scanner-indent s)))
    (push (scanner-indent s) (scanner-indents s))
    (setf (scanner-indent s) column)
    (if (= number -1)
        (enqueue-token s (make-token kind mark mark))
        (insert-token s (- number (scanner-tokens-parsed s))
                      (make-token kind mark mark))))
  t)

(defun unroll-indent (s column)
  "yaml_parser_unroll_indent."
  (unless (plusp (scanner-flow-level s))
    (loop while (> (scanner-indent s) column)
          do (enqueue-token s (make-token :block-end (sc-mark s) (sc-mark s)))
             (setf (scanner-indent s) (pop (scanner-indents s)))))
  t)

(defun scan-to-next-token (s)
  "yaml_parser_scan_to_next_token."
  (loop
    (when (and (zerop (scanner-column s)) (sc-bom-p s)) (sc-skip s))
    (when (and (zerop (scanner-flow-level s))
               (zerop (scanner-column s))
               (sc-tab-p s))
      (sc-error s "while scanning for the next token" (sc-mark s)
                "found a tab character where an indentation space is expected"))
    ;; YAML 1.2.2 permits tabs as non-indentation whitespace.
    (loop while (or (sc-space-p s) (sc-tab-p s))
          do (sc-skip s))
    (when (sc-check s #\#)
      (loop until (sc-breakz-p s) do (sc-skip s)))
    (if (sc-break-p s)
        (progn
          (sc-skip-line s)
          (when (zerop (scanner-flow-level s))
            (setf (scanner-simple-key-allowed s) t)))
        (return t))))
