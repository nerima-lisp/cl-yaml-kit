;;;; src/scanner-flow-scalars.lisp
(in-package #:yaml-kit)

(declaim (optimize (speed 3) (safety 1)))

(defparameter *flow-escape-table*
  ;; YAML 1.2.2 retains libyaml's \/ escape and accepts a literal tab escape.
  (let ((table (make-array 128 :initial-element nil)))
    (dolist (entry '((#\0 . 0) (#\a . 7) (#\b . 8) (#\t . 9) (#\Tab . 9)
                     (#\n . 10) (#\v . 11) (#\f . 12) (#\r . 13) (#\e . 27)
                     (#\Space . 32) (#\" . 34) (#\/ . 47) (#\\ . 92)
                     (#\N . #x85) (#\_ . #xa0) (#\L . #x2028) (#\P . #x2029)))
      (setf (aref table (char-code (car entry))) (cdr entry)))
    table))

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

(defun %scan-flow-scalar-body (s single-p start out leading trailing spaces leading-blanks)
  (let ((quote (if single-p #\' #\")))
    (loop
      (when (and (zerop (scanner-column s))
                 (or (and (sc-check s #\-) (sc-check s #\- 1) (sc-check s #\- 2))
                     (and (sc-check s #\.) (sc-check s #\. 1) (sc-check s #\. 2)))
                 (sc-blankz-p s 3))
        (sc-error s "while scanning a quoted scalar" start
                  "found unexpected document indicator"))
      (when (sc-z-p s)
        (sc-error s "while scanning a quoted scalar" start
                  "found unexpected end of stream"))
      (when (and leading-blanks
                 (< (scanner-column s) (1+ (scanner-indent s))))
        (sc-error s "while scanning a quoted scalar" start
                  "found a line that is not indented enough to continue the quoted scalar"))
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
      (when (sc-check s quote)
        (when leading-blanks
          (%flow-fold out leading trailing)
          (setf leading-blanks nil))
        (return))
      (loop while (or (sc-blank-p s) (sc-break-p s)) do
        (if (sc-blank-p s)
            (if leading-blanks (sc-skip s) (sc-read s spaces))
            (if leading-blanks
                (sc-read-line s trailing)
                (progn (setf (fill-pointer spaces) 0)
                       (sc-read-line s leading)
                       (setf leading-blanks t)))))
      (if leading-blanks (%flow-fold out leading trailing)
          (%flow-append-spaces out spaces)))
    leading-blanks))

(defun scan-flow-scalar (s single-p)
  (let ((start (sc-mark s)) (out (make-scan-buffer))
        (leading (make-scan-buffer)) (trailing (make-scan-buffer))
        (spaces (make-scan-buffer)) (leading-blanks nil))
    (sc-skip s)
    (%scan-flow-scalar-body s single-p start out leading trailing spaces leading-blanks)
    (sc-skip s)
    (when (sc-check s #\#)
      (sc-error s "while scanning a quoted scalar" start
                "found unexpected comment indicator"))
    (make-token :scalar start (sc-mark s) :value (scan-buffer-string out)
                :style (if single-p :single-quoted :double-quoted))))

(defun %scan-plain-scalar-body
  (s start out end leading trailing spaces leading-blanks indent)
  (let ((end-line (mark-line end))
        (end-column (mark-column end))
        (end-offset (mark-offset end)))
    (loop
      (when (and (zerop (scanner-column s))
                 (or (and (sc-check s #\-) (sc-check s #\- 1) (sc-check s #\- 2))
                     (and (sc-check s #\.) (sc-check s #\. 1) (sc-check s #\. 2)))
                 (sc-blankz-p s 3))
        (return))
      (when (and (zerop (scanner-flow-level s))
                 (zerop (scanner-column s))
                 (let ((character (sc-char s)))
                   (or (char= character #\-)
                       (char= character #\?)
                       (char= character #\:)))
                 (sc-blankz-p s 1))
        (return))
      (when (sc-check s #\#) (return))
      (loop while (not (sc-blankz-p s)) do
        (cond
          ((and (plusp (scanner-flow-level s))
                (sc-check s #\:)
                (or (sc-check s #\, 1) (sc-check s #\? 1)
                    (sc-check s #\[ 1) (sc-check s #\] 1)
                    (sc-check s #\{ 1) (sc-check s #\} 1)))
           (return))
          ((or (and (sc-check s #\:) (sc-blankz-p s 1))
               (and (plusp (scanner-flow-level s))
                    (or (sc-check s #\,) (sc-check s #\[)
                        (sc-check s #\]) (sc-check s #\{)
                        (sc-check s #\}))))
           (return)))
        (when (or leading-blanks (plusp (fill-pointer spaces)))
          (if leading-blanks
              (%flow-fold out leading trailing)
              (%flow-append-spaces out spaces))
          (setf leading-blanks nil))
        (sc-read s out)
        (setf end-line (scanner-line s)
              end-column (scanner-column s)
              end-offset (scanner-pos s)))
      (unless (or (sc-blank-p s) (sc-break-p s)) (return))
      (loop while (or (sc-blank-p s) (sc-break-p s)) do
        (if (sc-blank-p s)
            (if leading-blanks
                (progn
                  (when (and (zerop (scanner-flow-level s))
                             (< (scanner-column s) indent)
                             (sc-tab-p s))
                    (sc-error s "while scanning a plain scalar" start
                              "found a tab character that violates indentation"))
                  (sc-skip s))
                (sc-read s spaces))
            (if leading-blanks
                (sc-read-line s trailing)
                (progn
                  (setf (fill-pointer spaces) 0)
                  (sc-read-line s leading)
                  (setf leading-blanks t)))))
      (when (and (zerop (scanner-flow-level s))
                 (< (scanner-column s) indent))
        (return)))
    (values (make-mark end-line end-column end-offset) leading-blanks)))

(defun scan-plain-scalar (s)
  (let ((start (sc-mark s))
        (end (sc-mark s))
        (out (make-scan-buffer))
        (leading (make-scan-buffer))
        (trailing (make-scan-buffer))
        (spaces (make-scan-buffer))
        (leading-blanks nil)
        (indent (1+ (scanner-indent s))))
    (multiple-value-setq (end leading-blanks)
      (%scan-plain-scalar-body s start out end leading trailing spaces
                                leading-blanks indent))
    (when leading-blanks (setf (scanner-simple-key-allowed s) t))
    (make-token :scalar start end :value (scan-buffer-string out)
                :style :plain)))
