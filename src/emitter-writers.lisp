(in-package #:yaml-kit)

(defun %write-block-scalar (value context folded indent &optional (preserve-blank-indentation t))
  (declare (optimize (speed 3) (safety 1)) (type simple-string value))
  (let ((chomp (cond ((and (plusp (length value))
                           (%yaml-line-break-p (char value (1- (length value)))))
                      (if (and (> (length value) 1)
                               (%yaml-line-break-p (char value (- (length value) 2)))) "+" ""))
                     (t "-"))))
    (%emit-char context (if folded #\> #\|))
    (let ((first-content
            (loop for start = 0 then (1+ end)
                  for end = (position #\Newline value :start start)
                  for line = (subseq value start (or end (length value)))
                  unless (zerop (length line))
                    do (return line)
                  when (null end) do (return nil))))
      (when (and first-content
                 (or (%yaml-blank-p (char first-content 0))
                     (char= (char first-content 0) #\#)))
        (%emit-char context #\2)))
    (%emit-text context chomp)
    (%emit-char context #\Newline)
    (loop for start = 0 then (1+ end)
          for end = (position #\Newline value :start start)
          do (when (>= start (length value)) (return))
             (unless (or (= start (length value))
                         (and (not preserve-blank-indentation)
                              end (= start end))
                         (and end (= start end)
                              (let* ((next-start (1+ end))
                                     (next-end (and (< next-start (length value))
                                                   (position #\Newline value :start next-start))))
                                (or (null next-end)
                                    (= next-start next-end)
                                    (not (loop for index from next-start below next-end
                                               always (%yaml-blank-p (char value index))))))))
               (%emit-text context (make-string indent :initial-element #\Space)))
             (when end
               (%emit-text context (subseq value start end))
               (%emit-char context #\Newline))
             (unless end
               (when (< start (length value))
                 (%emit-text context (subseq value start)))
               (when (or (zerop (length value))
                         (not (%yaml-line-break-p (char value (1- (length value))))))
                 (%emit-char context #\Newline))
               (return)))))

(defun %blank-line-p (value start)
  (let ((end (position #\Newline value :start start)))
    (or (null end)
        (= start end)
        (loop for index from start below end
              always (%yaml-blank-p (char value index))))))

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

(defun %write-double-quoted (value context)
  (declare (optimize (speed 3) (safety 1)) (type simple-string value))
  (%emit-char context #\")
  (loop for character across value
        for code fixnum = (char-code character)
        do (let ((escape (cdr (assoc character
                                     '((#\Null . "\\0") (#\Bell . "\\a")
                                       (#\Backspace . "\\b") (#\Tab . "\\t")
                                       (#\Newline . "\\n") (#\Vt . "\\v")
                                       (#\Page . "\\f") (#\Return . "\\r")
                                       (#\Escape . "\\e") (#\" . "\\\"")
                                       (#\\ . "\\\\"))))))
             (cond (escape (%emit-text context escape))
                   ((= code #x85) (%emit-text context "\\N"))
                      ((= code #xa0) (%emit-text context "\\_"))
                      ((= code #x2028) (%emit-text context "\\L"))
                      ((= code #x2029) (%emit-text context "\\P"))
                      ((or (< code #x20) (= code #x7f) (= code #xfeff))
                       (%write-hex-escape context (if (= code #xfeff) "\\u" "\\x")
                                           code (if (= code #xfeff) 4 2)))
                      (t (%emit-char context character)))))
  (%emit-char context #\"))

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
