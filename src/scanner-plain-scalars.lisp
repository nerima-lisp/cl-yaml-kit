;;;; src/scanner-plain-scalars.lisp
(in-package #:yaml-kit)

(declaim (optimize (speed 3) (safety 1)))

(defun %plain-scalar-fast-end (s start-pos text length indent)
  (let ((index start-pos) (simple-end nil))
    (loop while (< index length)
          for character = (char text index)
          do (cond
               ((and (char= character +scanner-value-indicator+)
                     (sc-blankz-p s (1+ (- index start-pos))))
                (setf simple-end index)
                (return))
               ((yaml-whitespace-p character) (return nil))
               ((yaml-line-break-p character)
                (let ((next (1+ index))
                      (column 0))
                  (when (char= character #\Return)
                    (incf next))
                  (loop while (and (< next length)
                                   (yaml-whitespace-p (char text next)))
                        do (incf next)
                           (incf column))
                  (when (or (>= next length)
                            (and (not (char= (char text next) #\Newline))
                                 (< column indent)))
                    (setf simple-end index)
                    (return))
                  (return nil)))
               (t (incf index)))
          finally (setf simple-end index))
    (when (and simple-end
               (> index start-pos)
               (sc-printable-p s (- index start-pos)))
      index)))

(defun %scan-plain-scalar-fast (s start start-pos text length indent)
  (when (and (zerop (scanner-flow-level s))
             (not (and (zerop (scanner-column s))
                       (or (sc-check s +scanner-document-start-indicator+)
                           (sc-check s +scanner-key-indicator+)
                           (sc-check s +scanner-value-indicator+))
                       (sc-blankz-p s 1))))
    (let ((end-index (%plain-scalar-fast-end s start-pos text length indent)))
      (when end-index
        (loop repeat (- end-index start-pos) do (sc-skip s))
        (return-from %scan-plain-scalar-fast
          (make-scanner-token s :scalar start (sc-position s)
                             :value (subseq text start-pos end-index)
                             :style :plain)))))
  nil)

(defun %scan-plain-scalar-characters
    (s out leading trailing spaces leading-blanks end-line end-column end-offset)
  (loop while (not (sc-blankz-p s)) do
    (cond
      ((and (plusp (scanner-flow-level s))
            (sc-check s +scanner-value-indicator+)
            (member (sc-char s 1) +scanner-flow-value-terminators+))
       (return))
      ((or (and (sc-check s +scanner-value-indicator+)
                (sc-blankz-p s 1))
           (and (plusp (scanner-flow-level s))
                (member (sc-char s) +scanner-flow-plain-terminators+)))
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
  (values end-line end-column end-offset leading-blanks))

(defun %scan-plain-scalar-breaks
    (s start leading trailing spaces leading-blanks indent)
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
  leading-blanks)

(defun %scan-plain-scalar-body
    (s start out end leading trailing spaces leading-blanks indent)
  (let ((end-line (sc-position-line end))
        (end-column (sc-position-column end))
        (end-offset (sc-position-offset end)))
    (loop
      (when (and (zerop (scanner-column s))
                 (or (%scanner-document-indicator-p
                      s +scanner-document-start-indicator+)
                     (%scanner-document-indicator-p
                      s +scanner-document-end-indicator+)))
        (return))
      (when (and (zerop (scanner-flow-level s))
                 (zerop (scanner-column s))
                 (or (sc-check s +scanner-document-start-indicator+)
                     (sc-check s +scanner-key-indicator+)
                     (sc-check s +scanner-value-indicator+))
                 (sc-blankz-p s 1))
        (return))
      (when (sc-check s #\#) (return))
      (multiple-value-setq (end-line end-column end-offset leading-blanks)
        (%scan-plain-scalar-characters
         s out leading trailing spaces leading-blanks
         end-line end-column end-offset))
      (unless (or (sc-blank-p s) (sc-break-p s)) (return))
      (setf leading-blanks
            (%scan-plain-scalar-breaks
             s start leading trailing spaces leading-blanks indent))
      (when (and (zerop (scanner-flow-level s))
                 (< (scanner-column s) indent))
        (return)))
    (values (sc-position-at end-offset end-line end-column) leading-blanks)))

(defun scan-plain-scalar (s)
  (let* ((start (sc-position s))
         (start-pos (scanner-pos s))
         (text (scanner-text s))
         (length (length text))
         (indent (1+ (scanner-indent s)))
         (token (%scan-plain-scalar-fast s start start-pos text length indent)))
    (when token (return-from scan-plain-scalar token))
    (let ((end (sc-position s))
          (out (scanner-scan-buffer s))
          (leading (scanner-scan-leading s))
          (trailing (scanner-scan-trailing s))
          (spaces (scanner-scan-spaces s))
          (leading-blanks nil))
      (setf (fill-pointer out) 0
            (fill-pointer leading) 0
            (fill-pointer trailing) 0
            (fill-pointer spaces) 0)
      (multiple-value-setq (end leading-blanks)
        (%scan-plain-scalar-body s start out end leading trailing spaces
                                 leading-blanks indent))
      (when leading-blanks (setf (scanner-simple-key-allowed s) t))
      (make-scanner-token s :scalar start end :value (scan-buffer-string out)
                          :style :plain))))
