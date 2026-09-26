;;;; src/scanner-structure.lisp
(in-package #:yaml-kit)

(defun reader-line-start (state)
  (if (zerop (reader-state-position state)) 0
      (1+ (or (position #\Newline (reader-state-text state)
                         :end (reader-state-position state) :from-end t) -1))))

(defun reader-current-indent (state)
  (- (reader-state-position state) (reader-line-start state)))

(defun reader-skip-line (state)
  (loop until (or (reader-eof-p state) (yaml-line-break-p (reader-peek state)))
        do (reader-advance state))
  (when (and (not (reader-eof-p state)) (char= (reader-peek state) #\Return)) (reader-advance state))
  (when (and (not (reader-eof-p state)) (char= (reader-peek state) #\Newline)) (reader-advance state)))

(defun reader-skip-blank-lines (state)
  (loop
    (when (reader-eof-p state) (return))
    (let ((save (reader-state-position state)) (column (reader-state-column state)))
      (loop while (member (reader-peek state) '(#\Space #\Tab)) do (reader-advance state))
      (if (or (reader-eof-p state) (yaml-line-break-p (reader-peek state)) (char= (reader-peek state) #\#))
          (reader-skip-line state)
          (progn (setf (reader-state-position state) save (reader-state-column state) column) (return))))))

(defun reader-comment-or-break (state)
  (reader-skip-space state)
  (when (char= (or (reader-peek state) #\Null) #\#) (reader-skip-line state))
  (when (and (not (reader-eof-p state)) (yaml-line-break-p (reader-peek state))) (reader-skip-line state)))

(defun reader-count-indent (line)
  (loop for i fixnum from 0 below (length line)
        while (char= (char line i) #\Space) finally (return i)))
