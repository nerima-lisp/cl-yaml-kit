;;;; src/scanner-block-scalars.lisp
(in-package #:yaml-kit)

(declaim (optimize (speed 3) (safety 1)))

(defun scan-block-scalar-breaks (s indent breaks start-mark end-mark)
  (let ((max-indent 0))
    (setf (car end-mark) (sc-mark s))
    (loop
      (loop while (and (or (zerop (car indent))
                           (< (mark-column (sc-mark s)) (car indent)))
                       (sc-space-p s)) do (sc-skip s))
      (let ((column (mark-column (sc-mark s))))
        (when (> column max-indent) (setf max-indent column))
        ;; YAML 1.2.2 permits a tab in detected content indentation after
        ;; the content column is known; only an explicit lower indentation
        ;; makes a tab an indentation error.
        (when (and (< column (car indent))
                   (sc-tab-p s))
          (sc-error s "while scanning a block scalar" start-mark
                    "found a tab character where an indentation space is expected"))
        (unless (sc-break-p s)
          ;; The content indentation is the one the first non-empty line uses,
          ;; so a leading empty line indented further does not belong to it.
          (when (and (zerop (car indent)) (> max-indent column))
            (sc-error s "while scanning a block scalar" start-mark
                      "found a leading empty line indented more than the content"))
          (return)))
      (sc-read-line s breaks)
      (setf (car end-mark) (sc-mark s)))
    (when (zerop (car indent))
      ;; A root block scalar may start in column zero; nested scalars still
      ;; require one column beyond the parent indentation.
      (setf (car indent) (max max-indent (1+ (scanner-indent s)) 0)))
    nil))

(defun scan-block-scalar-header (s start-mark)
  (let ((chomping 0) (increment 0))
    (sc-skip s)
    (cond
      ((or (sc-check s #\+) (sc-check s #\-))
       (setf chomping (if (sc-check s #\+) 1 -1)) (sc-skip s)
       (when (sc-digit-p s)
         (when (sc-check s #\0) (sc-error s "while scanning a block scalar" start-mark
                                           "found an indentation indicator equal to 0"))
         (setf increment (- (char-code (sc-char s)) (char-code #\0))) (sc-skip s)))
      ((sc-digit-p s)
       (when (sc-check s #\0) (sc-error s "while scanning a block scalar" start-mark
                                         "found an indentation indicator equal to 0"))
       (setf increment (- (char-code (sc-char s)) (char-code #\0))) (sc-skip s)
       (when (or (sc-check s #\+) (sc-check s #\-))
         (setf chomping (if (sc-check s #\+) 1 -1)) (sc-skip s))))
    (when (sc-check s #\#) (sc-error s "while scanning a block scalar" start-mark
                                       "found unexpected character after block scalar indicator"))
    (loop while (sc-blank-p s) do (sc-skip s))
    (when (sc-check s #\#) (loop until (sc-breakz-p s) do (sc-skip s)))
    (unless (sc-breakz-p s) (sc-error s "while scanning a block scalar" start-mark
                                       "did not find expected comment or line break"))
    (when (sc-break-p s) (sc-skip-line s))
    (values chomping increment)))

(defun scan-block-scalar-content (s literal-p indent-cell start-mark end-cell)
  (let ((string (make-scan-buffer)) (leading-break (make-scan-buffer))
        (trailing-breaks (make-scan-buffer)) (leading-blank nil))
    (scan-block-scalar-breaks s indent-cell trailing-breaks start-mark end-cell)
    (loop while (and (= (mark-column (sc-mark s)) (car indent-cell)) (not (sc-z-p s))) do
      (when (and (zerop (mark-column (sc-mark s)))
                 (or (and (sc-check s #\-) (sc-check s #\- 1) (sc-check s #\- 2))
                     (and (sc-check s #\.) (sc-check s #\. 1) (sc-check s #\. 2)))
                 (sc-blankz-p s 3))
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
      (scan-block-scalar-breaks s indent-cell trailing-breaks start-mark end-cell))
    (values string leading-break trailing-breaks)))

(defun scan-block-scalar-chomping (string leading-break trailing-breaks chomping)
  (unless (= chomping -1) (loop for ch across leading-break do (vector-push-extend ch string)))
  (when (= chomping 1) (loop for ch across trailing-breaks do (vector-push-extend ch string)))
  string)

(defun scan-block-scalar (s literal-p)
  (let ((start-mark (sc-mark s)))
    (multiple-value-bind (chomping increment) (scan-block-scalar-header s start-mark)
      (let ((indent-cell (list (if (plusp increment)
                                   (+ (max 0 (scanner-indent s)) increment) 0)))
            (end-cell (list (sc-mark s))))
        (multiple-value-bind (string leading-break trailing-breaks)
            (scan-block-scalar-content s literal-p indent-cell start-mark end-cell)
          (make-token :scalar start-mark (car end-cell)
                      :value (scan-buffer-string
                              (scan-block-scalar-chomping string leading-break trailing-breaks chomping))
                      :style (if literal-p :literal :folded)))))))
