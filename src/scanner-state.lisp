;;;; src/scanner-state.lisp
(in-package #:yaml-kit)

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
