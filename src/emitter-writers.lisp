(in-package #:yaml-kit)

(progn
  (defun %block-scalar-chomping-indicator (value)
    (if (and (plusp (length value))
             (%yaml-scalar-line-break-p (char value (1- (length value)))))
        (if (and (> (length value) 1)
                 (%yaml-scalar-line-break-p (char value (- (length value) 2))))
            "+" "")
        "-"))

  (defun %block-scalar-first-content (value)
    (loop for start = 0 then (1+ end)
          for end = (position #\Newline value :start start)
          for line = (subseq value start (or end (length value)))
          unless (zerop (length line)) do (return line)
          when (null end) do (return nil)))

  (defun %write-block-scalar-header (value context folded)
    (%emit-char context (if folded #\> #\|))
    (let ((first-content (%block-scalar-first-content value)))
      (when (and first-content
                 (or (yaml-whitespace-p (char first-content 0))
                     (char= (char first-content 0) #\#)))
        (%emit-char context #\2)))
    (%emit-text context (%block-scalar-chomping-indicator value))
    (%emit-char context #\Newline))

  (defun %block-scalar-suppress-indent-p (value start end preserve-blank-indentation)
    (and end
         (= start end)
         (or (not preserve-blank-indentation)
             (let* ((next-start (1+ end))
                    (next-end (and (< next-start (length value))
                                   (position #\Newline value :start next-start))))
               (or (null next-end)
                   (= next-start next-end)
                   (not (loop for index from next-start below next-end
                              always (yaml-whitespace-p (char value index)))))))))

  (defun %write-block-scalar-line
      (value context start end indent preserve-blank-indentation)
    (unless (%block-scalar-suppress-indent-p value start end preserve-blank-indentation)
      (%emit-text context (make-string indent :initial-element #\Space)))
    (when end
      (%emit-text context (subseq value start end))
      (%emit-char context #\Newline))
    (unless end
      (when (< start (length value))
        (%emit-text context (subseq value start)))
      (%emit-char context #\Newline)))

  (defun %write-block-scalar-lines (value context indent preserve-blank-indentation)
    (loop for start = 0 then (1+ end)
          for end = (position #\Newline value :start start)
          do (when (>= start (length value)) (return))
             (%write-block-scalar-line value context start end indent
                                       preserve-blank-indentation)
             (unless end (return))))

  (defun %write-block-scalar
      (value context folded indent &optional (preserve-blank-indentation t))
    (declare (optimize (speed 3) (safety 1)) (type simple-string value))
    (%write-block-scalar-header value context folded)
    (%write-block-scalar-lines value context indent preserve-blank-indentation)))

(defun %write-single-quoted (value context &optional (indent 0))
  (declare (optimize (speed 3) (safety 1)) (type simple-string value))
  (%emit-char context #\')
  (let ((breaks nil))
    (loop for character across value do
      (if (char= character #\Newline)
          (progn
            (unless breaks (%emit-char context #\Newline))
            (%emit-char context #\Newline)
            (setf breaks t))
          (progn
            (when breaks
              (%emit-text context (make-string indent :initial-element #\Space))
              (setf breaks nil))
            (%emit-char context character)
            (when (char= character #\') (%emit-char context #\')))))
    (when breaks
      (%emit-text context (make-string indent :initial-element #\Space))))
  (%emit-char context #\'))

(defun %write-plain (value context)
  (declare (optimize (speed 3) (safety 1)) (type simple-string value))
  (loop for start = 0 then (1+ end)
        for end = (position #\Newline value :start start)
        do (if end
               (progn
                 (when (> end start)
                   (%emit-text context (subseq value start end)))
                 (%emit-char context #\Newline)
                 (%emit-char context #\Newline))
               (progn
                 (when (< start (length value))
                   (%emit-text context (subseq value start)))
                 (return)))))

(progn
  (defun %write-non-printable-escape (context code)
    (cond ((<= code #xff) (%write-hex-escape context "\\x" code 2))
          (t (%write-hex-escape context "\\u" code 4))))

  (defun %write-double-quoted (value context)
    (declare (optimize (speed 3) (safety 1)) (type simple-string value))
    (%emit-char context #\")
    (loop for character across value
          for code fixnum = (char-code character)
          for escape = (gethash character *yaml-escape-table*)
          do (cond (escape (%emit-text context escape))
                   ((or (= code #xfeff) (not (yaml-printable-p character)))
                    (%write-non-printable-escape context code))
                   (t (%emit-char context character))))
    (%emit-char context #\")))

(defun %write-hex-escape (context prefix code digits)
  (%emit-text context prefix)
  (loop for shift downfrom (* 4 (1- digits)) to 0 by 4
        do (%emit-char context (%hex-digit (logand #xf (ash code (- shift)))))))

(defun %write-scalar (value style context &optional (indent 0)
                                              (preserve-blank-indentation t))
  (case style
    (:plain (%write-plain value context))
    (:single-quoted (%write-single-quoted value context indent))
    (:double-quoted (%write-double-quoted value context))
    (:literal (%write-block-scalar value context nil indent preserve-blank-indentation))
    (:folded (%write-block-scalar value context t indent preserve-blank-indentation))))
