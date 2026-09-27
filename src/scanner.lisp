;;;; src/scanner.lisp
(in-package #:yaml-kit)

(defconstant +max-simple-key-length+ 1024)

(declaim (inline scanner-queue-nonempty-p))
(defun scanner-queue-nonempty-p (s)
  (< (scanner-tokens-head s) (fill-pointer (scanner-tokens s))))

(defun scanner-peek-token (s)
  (when (scanner-stream-end-produced s) (return-from scanner-peek-token nil))
  (unless (scanner-token-available s) (fetch-more-tokens s))
  (when (scanner-queue-nonempty-p s)
    (aref (scanner-tokens s) (scanner-tokens-head s))))

(defun scanner-next-token (s)
  (let ((token (scanner-peek-token s)))
    (when token
      (incf (scanner-tokens-head s))
      (incf (scanner-tokens-parsed s))
      (when (scanner-token-recycling-enabled s)
        (setf (aref (scanner-tokens s) (1- (scanner-tokens-head s))) nil
              (scanner-recyclable-token s) token))
      (setf (scanner-token-available s) nil)
      (when (eq (token-kind token) :stream-end)
        (setf (scanner-stream-end-produced s) t))
      (when (and (> (scanner-tokens-head s) 64)
                 (> (* 2 (scanner-tokens-head s))
                    (fill-pointer (scanner-tokens s))))
        (let* ((tokens (scanner-tokens s))
               (remaining (- (fill-pointer tokens) (scanner-tokens-head s))))
          (replace tokens tokens :start1 0 :start2 (scanner-tokens-head s)
                   :end2 (fill-pointer tokens))
          (setf (fill-pointer tokens) remaining
                (scanner-tokens-head s) 0)))
      token)))

(defun scanner-token-limit (s)
  "Upper bound on tokens this scanner may produce for its input.
Block collections emit :block-end and inserted :key tokens that consume no
input, so the bound is a multiple of the input length rather than the length."
  (+ 64 (* 8 (length (scanner-text s)))))

(defun recycle-scanner-token (s)
  (when (and (scanner-token-recycling-enabled s)
             (scanner-recyclable-token s))
    (push (scanner-recyclable-token s) (scanner-token-pool s))
    (setf (scanner-recyclable-token s) nil))
  t)

(defun scanner-resource-error (s name limit actual)
  (error 'yaml-resource-limit-error
         :limit-name name :limit limit :actual actual :mark (sc-mark s)))

(defun fetch-more-tokens (s)
  (when (scanner-stream-end-produced s) (return-from fetch-more-tokens nil))
  (let ((limit (scanner-token-limit s)))
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
        (if (scanner-token-recycling-enabled s)
            (let ((*token-pool* (scanner-token-pool s)))
              (fetch-next-token s)
              (setf (scanner-token-pool s) *token-pool*))
            (fetch-next-token s))
        ;; A fetch that stops advancing the position would otherwise enqueue
        ;; tokens until the heap dies, so bound the queue by the input size.
        (let ((produced (fill-pointer (scanner-tokens s))))
          (when (> produced limit)
            (scanner-resource-error s "tokens" limit produced)))))))

(defun fetch-next-token (s)
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
      ((and (plusp (scanner-flow-level s)) (char= c #\]))
       (fetch-flow-collection-end s :flow-sequence-end))
      ((and (plusp (scanner-flow-level s)) (char= c #\}))
       (fetch-flow-collection-end s :flow-mapping-end))
      ((and (plusp (scanner-flow-level s)) (char= c #\,)) (fetch-flow-entry s))
      ((and (char= c #\-)
            (or (sc-blankz-p s 1)
                (and (plusp (scanner-flow-level s))
                     (member c1 '(#\, #\] #\})))))
       (fetch-block-entry s))
      ((and (char= c #\?)
            (or (sc-blankz-p s 1)
                (and (plusp (scanner-flow-level s))
                     (yaml-flow-indicator-p c1))))
       (fetch-key s))
      ;; YAML 1.2.2 has two value indicators in flow context.
      ;; c-ns-flow-map-separate-value needs separation after ":", and
      ;; c-ns-flow-map-adjacent-value follows a c-flow-json-node whose closing
      ;; indicator makes the ":" unambiguous.  libyaml only tests flow_level
      ;; against the next character, which rejects every adjacent JSON key.
      ((and (char= c #\:)
            (or (sc-blankz-p s 1)
                (and (plusp (scanner-flow-level s))
                     (or (member c1 '(#\, #\? #\[ #\] #\{ #\}))
                         (scanner-json-like-node-end s)))))
       (fetch-value s))
      ((char= c #\*) (fetch-anchor s :alias))
      ((char= c #\&) (fetch-anchor s :anchor))
      ((char= c #\!) (fetch-tag s))
      ((and (char= c #\|) (zerop (scanner-flow-level s))) (fetch-block-scalar s t))
      ((and (char= c #\>) (zerop (scanner-flow-level s))) (fetch-block-scalar s nil))
      ((char= c #\') (fetch-flow-scalar s t))
      ((char= c #\") (fetch-flow-scalar s nil))
      ((or (not (or (sc-blankz-p s) (yaml-indicator-p c)))
           (and (zerop (scanner-flow-level s))
                (member c '(#\, #\[ #\] #\{ #\})))
           (and (char= c #\-) (not (sc-blank-p s 1)))
           (and (member c '(#\? #\:))
                (not (sc-blankz-p s 1))))
       (fetch-plain-scalar s))
      (t (sc-error s "while scanning for the next token" (sc-mark s)
                   "found character that cannot start any token")))))
