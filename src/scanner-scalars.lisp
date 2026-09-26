;;;; src/scanner-scalars.lisp
(in-package #:yaml-kit)

(defun reader-decode-double-quoted (text)
  (with-output-to-string (out)
    (loop for i fixnum from 0 below (length text)
          for c = (char text i)
          do (if (char= c #\\)
                 (progn
                   (incf i)
                   (when (>= i (length text)) (error 'yaml-parse-error :context "unterminated escape"))
                   (let ((e (char text i)))
                     (case e
                       (#\n (write-char #\Newline out)) (#\r (write-char #\Return out))
                       (#\t (write-char #\Tab out)) (#\0 (write-char (code-char 0) out))
                       (#\" (write-char #\" out)) (#\\ (write-char #\\ out))
                       (#\/ (write-char #\/ out))
                       (otherwise (write-char e out)))))
                 (write-char c out)))))
