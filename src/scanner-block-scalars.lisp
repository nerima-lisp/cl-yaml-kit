;;;; src/scanner-block-scalars.lisp
(in-package #:yaml-kit)

(declaim (optimize (speed 3) (safety 1)))

(defun scan-block-scalar-breaks (s indent breaks start-mark end-mark)
  "yaml_parser_scan_block_scalar_breaks."
  (let ((max-indent 0))
    (setf (car end-mark) (sc-mark s))
    (loop
      (loop while (and (or (zerop (car indent))
                           (< (mark-column (sc-mark s)) (car indent)))
                       (or (sc-space-p s) (sc-tab-p s))) do
        (when (sc-tab-p s)
          (sc-error s "while scanning a block scalar" start-mark
                    "found a tab character where an indentation space is expected"))
        (sc-skip s))
      (let ((column (mark-column (sc-mark s))))
        (when (> column max-indent) (setf max-indent column)))
      (unless (sc-break-p s) (return))
      (sc-read-line s breaks)
      (setf (car end-mark) (sc-mark s)))
    (when (zerop (car indent))
      (setf (car indent) (max max-indent (1+ (scanner-indent s)) 1)))
    nil))

(defun scan-block-scalar (s literal-p)
  "yaml_parser_scan_block_scalar."
  (let ((start-mark (sc-mark s))
        (string (make-scan-buffer))
        (leading-break (make-scan-buffer))
        (trailing-breaks (make-scan-buffer))
        (chomping 0) (increment 0) (leading-blank nil))
    ;; YAML 1.2.2 permits the two indicators in either order.
    (sc-skip s)
    (cond
      ((or (sc-check s #\+) (sc-check s #\-))
       (setf chomping (if (sc-check s #\+) 1 -1)) (sc-skip s)
       (when (sc-digit-p s)
         (when (sc-check s #\0)
           (sc-error s "while scanning a block scalar" start-mark
                     "found an indentation indicator equal to 0"))
         (setf increment (- (char-code (sc-char s)) (char-code #\0)))
         (sc-skip s)))
      ((sc-digit-p s)
       (when (sc-check s #\0)
         (sc-error s "while scanning a block scalar" start-mark
                   "found an indentation indicator equal to 0"))
       (setf increment (- (char-code (sc-char s)) (char-code #\0))) (sc-skip s)
       (when (or (sc-check s #\+) (sc-check s #\-))
         (setf chomping (if (sc-check s #\+) 1 -1)) (sc-skip s))))
    (loop while (sc-blank-p s) do (sc-skip s))
    (when (sc-check s #\#)
      (loop until (sc-breakz-p s) do (sc-skip s)))
    (unless (sc-breakz-p s)
      (sc-error s "while scanning a block scalar" start-mark
                "did not find expected comment or line break"))
    (when (sc-break-p s) (sc-skip-line s))
    (let ((indent-cell (list (if (plusp increment)
                                 (+ (max 0 (scanner-indent s)) increment) 0)))
          (end-cell (list (sc-mark s))))
      (scan-block-scalar-breaks s indent-cell trailing-breaks start-mark end-cell)
      (loop while (and (= (mark-column (sc-mark s)) (car indent-cell))
                       (not (sc-z-p s))) do
        (let ((trailing-blank (sc-blank-p s)))
          (if (and (not literal-p) (plusp (fill-pointer leading-break))
                   (char= (char leading-break 0) #\Newline)
                   (not leading-blank) (not trailing-blank))
              (progn
                (when (zerop (fill-pointer trailing-breaks))
                  (vector-push-extend #\Space string))
                (setf (fill-pointer leading-break) 0))
              (progn
                (loop for ch across leading-break do (vector-push-extend ch string))
                (setf (fill-pointer leading-break) 0))))
        (loop for ch across trailing-breaks do (vector-push-extend ch string))
        (setf (fill-pointer trailing-breaks) 0
              leading-blank (sc-blank-p s))
        (loop while (not (sc-breakz-p s)) do (sc-read s string))
        (when (sc-break-p s) (sc-read-line s leading-break))
        (scan-block-scalar-breaks s indent-cell trailing-breaks start-mark end-cell))
      (unless (= chomping -1)
        (loop for ch across leading-break do (vector-push-extend ch string)))
      (when (= chomping 1)
        (loop for ch across trailing-breaks do (vector-push-extend ch string)))
      (make-token :scalar start-mark (car end-cell)
                  :value (scan-buffer-string string)
                  :style (if literal-p :literal :folded)))))
