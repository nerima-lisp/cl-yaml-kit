;;;; src/scanner-flow-scalars.lisp
(in-package #:yaml-kit)

(declaim (optimize (speed 3) (safety 1)))

(defun %flow-valid-code-point-p (value)
  (and (< value char-code-limit) (<= value #x10ffff)
       (not (<= #xd800 value #xdfff))))

(defun %flow-fold (out leading trailing)
  (if (and (plusp (fill-pointer leading))
           (char= (aref leading 0) #\Newline))
      (if (zerop (fill-pointer trailing))
          (vector-push-extend #\Space out)
          (loop for i below (fill-pointer trailing) do
            (vector-push-extend (aref trailing i) out)))
      (progn
        (loop for i below (fill-pointer leading) do
          (vector-push-extend (aref leading i) out))
        (loop for i below (fill-pointer trailing) do
          (vector-push-extend (aref trailing i) out))))
  (setf (fill-pointer leading) 0 (fill-pointer trailing) 0))

(defun %flow-escaped-break (s)
  (sc-skip s) (sc-skip-line s)
  (loop while (sc-blank-p s) do (sc-skip s)))

(defun %flow-escape (s out start)
  (sc-skip s)
  (let ((c (sc-char s)))
    (cond
      ((and (< (char-code c) 128)
            (not (null (aref *flow-escape-table* (char-code c)))))
       (vector-push-extend (code-char (aref *flow-escape-table* (char-code c))) out)
       (sc-skip s))
      ((member c '(#\x #\u #\U))
         (let ((n (case c (#\x 2) (#\u 4) (t 8))) (value 0))
           (dotimes (i n)
           (unless (sc-hex-p s (1+ i))
             (sc-error s "while parsing a quoted scalar" start
                       "did not find expected hexadecimal number"))
           (setf value (+ (* value 16) (sc-hex-value s (1+ i)))))
         (unless (%flow-valid-code-point-p value)
           (sc-error s "while parsing a quoted scalar" start
                     "found invalid Unicode character escape code"))
         (vector-push-extend (code-char value) out)
         (sc-skip s)
         (loop repeat n do (sc-skip s))))
      ((sc-break-p s) (%flow-escaped-break s))
      (t (sc-error s "while parsing a quoted scalar" start
                  "found unknown escape character")))))

(defun %flow-append-spaces (out spaces)
  (loop for i below (fill-pointer spaces) do
    (vector-push-extend (aref spaces i) out))
  (setf (fill-pointer spaces) 0))

(defun %scan-flow-scalar-characters
    (s single-p quote start out leading trailing leading-blanks)
  (loop while (not (sc-blankz-p s)) do
    (when leading-blanks
      (%flow-fold out leading trailing)
      (setf leading-blanks nil))
    (cond ((and single-p (sc-check s #\') (sc-check s #\' 1))
           (vector-push-extend #\' out) (sc-skip s) (sc-skip s))
          ((sc-check s quote) (return))
          ((and (not single-p) (sc-check s #\\) (sc-break-p s 1))
           (%flow-escaped-break s) (setf leading-blanks t) (return))
          ((and (not single-p) (sc-check s #\\))
           (%flow-escape s out start))
          (t (sc-read s out))))
  leading-blanks)

(defun %scan-flow-scalar-spaces (s out leading trailing spaces leading-blanks)
  (loop while (or (sc-blank-p s) (sc-break-p s)) do
    (if (sc-blank-p s)
        (if leading-blanks (sc-skip s) (sc-read s spaces))
        (if leading-blanks
            (sc-read-line s trailing)
            (progn (setf (fill-pointer spaces) 0)
                   (sc-read-line s leading)
                   (setf leading-blanks t)))))
  (if leading-blanks (%flow-fold out leading trailing)
      (%flow-append-spaces out spaces))
  leading-blanks)

(defun %scan-flow-scalar-body (s single-p start out leading trailing spaces leading-blanks)
  (let ((quote (if single-p #\' #\")))
    (loop
      (when (and (zerop (scanner-column s))
                 (or (%scanner-document-indicator-p
                      s +scanner-document-start-indicator+)
                     (%scanner-document-indicator-p
                      s +scanner-document-end-indicator+)))
        (sc-error s "while scanning a quoted scalar" start
                  "found unexpected document indicator"))
      (when (sc-z-p s)
        (sc-error s "while scanning a quoted scalar" start
                  "found unexpected end of stream"))
      (when (and leading-blanks
                 (< (scanner-column s) (1+ (scanner-indent s))))
        (sc-error s "while scanning a quoted scalar" start
                  "found a line that is not indented enough to continue the quoted scalar"))
      (setf leading-blanks
            (%scan-flow-scalar-characters
             s single-p quote start out leading trailing leading-blanks))
      (when (sc-check s quote)
        (when leading-blanks
          (%flow-fold out leading trailing)
          (setf leading-blanks nil))
        (return))
      (setf leading-blanks
            (%scan-flow-scalar-spaces
             s out leading trailing spaces leading-blanks)))
    leading-blanks))

(defun scan-flow-scalar (s single-p)
  (let ((start (sc-position s)) (out (scanner-scan-buffer s))
        (leading (scanner-scan-leading s))
        (trailing (scanner-scan-trailing s))
        (spaces (scanner-scan-spaces s)) (leading-blanks nil))
    (setf (fill-pointer out) 0
          (fill-pointer leading) 0
          (fill-pointer trailing) 0
          (fill-pointer spaces) 0)
    (sc-skip s)
    (%scan-flow-scalar-body s single-p start out leading trailing spaces leading-blanks)
    (sc-skip s)
    (when (sc-check s #\#)
      (sc-error s "while scanning a quoted scalar" start
                "found unexpected comment indicator"))
    (make-scanner-token s :scalar start (sc-position s) :value (scan-buffer-string out)
                :style (if single-p :single-quoted :double-quoted))))
