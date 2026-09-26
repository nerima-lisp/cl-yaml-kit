;;;; src/scanner-state.lisp
(in-package #:yaml-kit)

(defstruct (scanner (:constructor %make-scanner))
  (text "" :type simple-string)
  (position 0 :type fixnum)
  (line 1 :type fixnum)
  (column 1 :type fixnum)
  (tokens nil :type list)
  (tail nil :type list)
  (flow-level 0 :type fixnum)
  (indents nil :type list)
  (simple-keys nil :type list)
  (last-token-number 0 :type fixnum))

(declaim (inline scan-eof-p scan-peek scan-mark scan-advance scan-error))
(declaim (ftype function scan-tokenize))

(defun scan-eof-p (scanner)
  (>= (scanner-position scanner) (length (scanner-text scanner))))

(defun scan-peek (scanner &optional (delta 0))
  (let ((index (+ (scanner-position scanner) delta)))
    (and (< index (length (scanner-text scanner)))
         (char (scanner-text scanner) index))))

(defun scan-mark (scanner)
  (make-mark (scanner-line scanner) (scanner-column scanner)
             (scanner-position scanner)))

(defun scan-advance (scanner)
  (unless (scan-eof-p scanner)
    (let ((character (scan-peek scanner)))
      (incf (scanner-position scanner))
      (if (char= character #\Newline)
          (progn (incf (scanner-line scanner)) (setf (scanner-column scanner) 1))
          (incf (scanner-column scanner)))))
  nil)

(defun scan-error (scanner context)
  (error 'yaml-parse-error :line (scanner-line scanner)
         :column (scanner-column scanner) :offset (scanner-position scanner)
         :context context))

(defun scan-push-token (scanner kind start end &key value handle suffix style major minor)
  (let ((token (make-token kind start end :value value :handle handle :suffix suffix
                           :style style :major major :minor minor)))
    (if (scanner-tokens scanner)
        (setf (cdr (scanner-tail scanner)) (list token)
              (scanner-tail scanner) (cdr (scanner-tail scanner)))
        (setf (scanner-tokens scanner) (list token)
              (scanner-tail scanner) (scanner-tokens scanner)))
    (incf (scanner-last-token-number scanner))
    token))

(defun make-scanner (text)
  (check-type text (simple-array character (*)))
  (let ((scanner (%make-scanner :text text)))
    (scan-tokenize scanner)
    scanner))

(defun scanner-peek-token (scanner)
  (car (scanner-tokens scanner)))

(defun scanner-next-token (scanner)
  (let ((tokens (scanner-tokens scanner)))
    (when tokens
      (prog1 (car tokens)
        (setf (scanner-tokens scanner) (cdr tokens))
        (unless (scanner-tokens scanner) (setf (scanner-tail scanner) nil))))))

;;; Legacy reader state remains available until the token parser is connected.
(defstruct (reader-state (:constructor make-reader-state%))
  (text "" :type simple-string)
  (position 0 :type fixnum)
  (line 1 :type fixnum)
  (column 1 :type fixnum)
  (max-input-length 104857600 :type fixnum)
  (max-depth 256 :type fixnum)
  (max-scalar-length 16777216 :type fixnum)
  (handler nil)
  (depth 0 :type fixnum)
  (document-open nil :type boolean))

(declaim (inline reader-eof-p reader-peek reader-mark))

(defun reader-eof-p (state)
  (>= (reader-state-position state) (length (reader-state-text state))))

(defun reader-peek (state &optional (delta 0))
  (let ((index (+ (reader-state-position state) delta)))
    (and (< index (length (reader-state-text state)))
         (char (reader-state-text state) index))))

(defun reader-mark (state)
  (make-mark (reader-state-line state) (reader-state-column state)
             (reader-state-position state)))

(defun reader-advance (state)
  (unless (reader-eof-p state)
    (let ((character (reader-peek state)))
      (incf (reader-state-position state))
      (if (char= character #\Newline)
          (progn (incf (reader-state-line state)) (setf (reader-state-column state) 1))
          (incf (reader-state-column state)))))
  nil)

(defun reader-parse-error (state context)
  (error 'yaml-parse-error :line (reader-state-line state)
         :column (reader-state-column state) :offset (reader-state-position state)
         :context context))

(defun reader-limit-depth (state)
  (when (> (reader-state-depth state) (reader-state-max-depth state))
    (error 'yaml-resource-limit-error)))
