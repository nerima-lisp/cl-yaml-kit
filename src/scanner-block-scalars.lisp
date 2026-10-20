;;;; src/scanner-block-scalars.lisp
(in-package #:yaml-kit)

(declaim (optimize (speed 3) (safety 1)))

(defun scan-block-scalar-breaks (s indent breaks start-position end-position)
  (let ((max-indent 0))
    (setf (car end-position) (sc-position s))
    (loop
      (loop while (and (or (zerop (car indent))
                           (< (scanner-column s) (car indent)))
                       (sc-space-p s)) do (sc-skip s))
      (let ((column (scanner-column s)))
        (when (> column max-indent) (setf max-indent column))
        ;; YAML 1.2.2 allows tabs once scalar content begins. A tab before
        ;; the content column is an indentation error.
        (when (and (< column (car indent))
                   (sc-tab-p s))
          (sc-error s "while scanning a block scalar" start-position
                    "found a tab character where an indentation space is expected"))
        (unless (sc-break-p s)
          ;; Use the first non-empty line to detect content indentation. A
          ;; preceding blank line cannot increase it.
          (when (and (zerop (car indent)) (> max-indent column))
            (sc-error s "while scanning a block scalar" start-position
                      "found a leading empty line indented more than the content"))
          (return)))
      (sc-read-line s breaks)
      (setf (car end-position) (sc-position s)))
    (when (zerop (car indent))
      ;; A root block scalar may start in column zero; nested scalars still
      ;; require one column beyond the parent indentation.
      (setf (car indent) (max max-indent (1+ (scanner-indent s)) 0)))
    nil))

(defun scan-block-scalar-header (s start-position)
  (let ((chomping 0) (increment 0))
    (sc-skip s)
    (cond
      ((or (sc-check s #\+) (sc-check s #\-))
       (setf chomping (if (sc-check s #\+) 1 -1))
       (sc-skip s)
       (when (sc-digit-p s)
         (when (sc-check s #\0)
           (sc-error s "while scanning a block scalar" start-position
                     "found an indentation indicator equal to 0"))
         (setf increment (- (char-code (sc-char s)) (char-code #\0)))
         (sc-skip s)))
      ((sc-digit-p s)
       (when (sc-check s #\0)
         (sc-error s "while scanning a block scalar" start-position
                   "found an indentation indicator equal to 0"))
       (setf increment (- (char-code (sc-char s)) (char-code #\0)))
       (sc-skip s)
       (when (or (sc-check s #\+) (sc-check s #\-))
         (setf chomping (if (sc-check s #\+) 1 -1))
         (sc-skip s))))
    (when (sc-check s #\#)
      (sc-error s "while scanning a block scalar" start-position
                "found unexpected character after block scalar indicator"))
    (loop while (sc-blank-p s) do (sc-skip s))
    (when (sc-check s #\#) (sc-skip-comment s))
    (unless (sc-breakz-p s)
      (sc-error s "while scanning a block scalar" start-position
                "did not find expected comment or line break"))
    (when (sc-break-p s) (sc-skip-line s))
    (values chomping increment)))

(defun scan-block-scalar-content (s literal-p indent-cell start-position end-cell)
  (let ((string (make-scan-buffer)) (leading-break (make-scan-buffer))
        (trailing-breaks (make-scan-buffer)) (leading-blank nil))
    (scan-block-scalar-breaks s indent-cell trailing-breaks start-position end-cell)
    (loop while (and (= (scanner-column s) (car indent-cell)) (not (sc-z-p s))) do
      (when (and (zerop (scanner-column s))
                 (or (%scanner-document-indicator-p
                      s +scanner-document-start-indicator+)
                     (%scanner-document-indicator-p
                      s +scanner-document-end-indicator+)))
        (return))
      (let ((trailing-blank (sc-blank-p s)))
        (if (and (not literal-p) (plusp (fill-pointer leading-break))
                 (char= (char leading-break 0) #\Newline) (not leading-blank) (not trailing-blank))
            (progn (when (zerop (fill-pointer trailing-breaks)) (vector-push-extend #\Space string))
                   (setf (fill-pointer leading-break) 0))
            (progn (loop for ch across leading-break do (vector-push-extend ch string))
                   (setf (fill-pointer leading-break) 0))))
      (loop for ch across trailing-breaks do (vector-push-extend ch string))
      (setf (fill-pointer trailing-breaks) 0 leading-blank (sc-blank-p s))
      (loop while (not (sc-breakz-p s)) do (sc-read s string))
      (when (sc-break-p s) (sc-read-line s leading-break))
      (scan-block-scalar-breaks s indent-cell trailing-breaks start-position end-cell))
    (values string leading-break trailing-breaks)))

(defun scan-block-scalar-chomping (string leading-break trailing-breaks chomping)
  (unless (= chomping -1) (loop for ch across leading-break do (vector-push-extend ch string)))
  (when (= chomping 1) (loop for ch across trailing-breaks do (vector-push-extend ch string)))
  string)

(defun scan-block-scalar (s literal-p)
  (let ((start-position (sc-position s)))
    (multiple-value-bind (chomping increment) (scan-block-scalar-header s start-position)
      (let ((indent-cell (list (if (plusp increment)
                                   (+ (max 0 (scanner-indent s)) increment) 0)))
            (end-cell (list (sc-position s))))
        (multiple-value-bind (string leading-break trailing-breaks)
            (scan-block-scalar-content s literal-p indent-cell start-position end-cell)
          (make-scanner-token s :scalar start-position (car end-cell)
                      :value (scan-buffer-string
                              (scan-block-scalar-chomping string leading-break trailing-breaks chomping))
                      :style (if literal-p :literal :folded)))))))
