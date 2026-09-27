(in-package #:yaml-kit)
(declaim (optimize (speed 3) (safety 1)))
(defconstant +max-version-number-length+ 9)

(defun %skip-reserved-directive-line (s)
  (loop until (sc-breakz-p s) do (sc-skip s))
  (when (sc-break-p s) (sc-skip-line s)))

(defun %skip-directive-gap (s)
  (loop
    (loop while (sc-blank-p s) do (sc-skip s))
    (cond
      ((sc-break-p s) (%skip-reserved-directive-line s))
      ((sc-check s #\#) (%skip-reserved-directive-line s))
      (t (return)))))

(defun %reserved-directive-result (s start)
  (cond
    ((sc-check s #\%) nil)
    ((and (sc-check s #\-) (sc-check s #\- 1) (sc-check s #\- 2)
          (sc-blankz-p s 3))
     (let ((document-start (sc-mark s)))
       (dotimes (i 3) (sc-skip s))
       (make-token :document-start document-start (sc-mark s))))
    (t (sc-error s "while scanning a directive" start
                 "found reserved directive name"))))

(defun scan-directive (s)
  (loop
    (let ((start (sc-mark s)))
      (sc-skip s)
      (let ((name (scan-directive-name s start)))
        (cond
        ((string= name "YAML")
         (multiple-value-bind (major minor) (scan-version-directive-value s start)
           ;; YAML 1.2.2 only defines the 1.x version family.
           (unless (= major 1) (sc-error s "while scanning a %YAML directive" start
                                         "found incompatible YAML major version"))
           (let ((end (sc-mark s))) (scan-directive-end s start)
             (return (make-token :version-directive start end :major major :minor minor)))))
        ((string= name "TAG")
         (multiple-value-bind (handle prefix) (scan-tag-directive-value s start)
           (let ((end (sc-mark s))) (scan-directive-end s start)
             (return (make-token :tag-directive start end :handle handle :value prefix)))))
          (t
           ;; YAML 1.2.2 reserves unknown directives; consume their line.
           (%skip-reserved-directive-line s)
           ;; Comments and blank lines remain part of the inter-token gap.
           (%skip-directive-gap s)
           (let ((result (%reserved-directive-result s start)))
             (if result (return result)))))))))

(defun scan-directive-end (s start)
  (loop while (sc-blank-p s) do (sc-skip s))
  (when (sc-check s #\#) (loop until (sc-breakz-p s) do (sc-skip s)))
  (unless (sc-breakz-p s) (sc-error s "while scanning a directive" start
                                      "did not find expected comment or line break"))
  (when (sc-break-p s) (sc-skip-line s)))

(defun scan-directive-name (s start)
  (let ((b (make-scan-buffer)))
    (loop while (sc-alpha-p s) do (sc-read s b))
    (when (zerop (fill-pointer b)) (sc-error s "while scanning a directive" start
                                               "could not find expected directive name"))
    (unless (sc-blankz-p s) (sc-error s "while scanning a directive" start
                                      "found unexpected non-alphabetical character"))
    (scan-buffer-string b)))

(defun scan-version-directive-value (s start)
  (loop while (sc-blank-p s) do (sc-skip s))
  (let ((major (scan-version-directive-number s start)))
    (unless (sc-check s #\.) (sc-error s "while scanning a %YAML directive" start
                                        "did not find expected digit or '.' character"))
    (sc-skip s) (values major (scan-version-directive-number s start))))

(defun scan-version-directive-number (s start)
  (let ((value 0) (length 0))
    (loop while (sc-digit-p s) do
      (incf length)
      (when (> length +max-version-number-length+)
        (sc-error s "while scanning a %YAML directive" start "found extremely long version number"))
      (setf value (+ (* value 10) (sc-hex-value s 0))) (sc-skip s))
    (when (zerop length) (sc-error s "while scanning a %YAML directive" start
                                    "did not find expected version number"))
    value))

(defun scan-tag-directive-value (s start)
  (loop while (sc-blank-p s) do (sc-skip s))
  (let ((handle (scan-tag-handle s t start)))
    (unless (sc-blank-p s) (sc-error s "while scanning a %TAG directive" start
                                      "did not find expected whitespace"))
    (loop while (sc-blank-p s) do (sc-skip s))
    (let ((prefix (scan-tag-uri s t t nil start)))
      (unless (sc-blankz-p s) (sc-error s "while scanning a %TAG directive" start
                                         "did not find expected whitespace or line break"))
      (values handle prefix))))

(defun scan-anchor (s kind)
  (let ((start (sc-mark s)) (b (make-scan-buffer)))
    (sc-skip s)
    ;; YAML 1.2.2 defines ns-anchor-char as ns-char minus c-flow-indicator, and
    ;; c-flow-indicator is only ",[]{}".  A ":" or "?" is an ordinary anchor
    ;; character, which is what makes "&a: key" an anchor named "a:" rather than
    ;; a mapping with an anchored key.
    (loop while (and (sc-printable-p s)
                     (not (sc-bom-p s))
                     (not (sc-blankz-p s))
                     (not (yaml-flow-indicator-p (sc-char s))))
          do (sc-read s b))
    (when (and (zerop (scanner-flow-level s))
               (yaml-flow-indicator-p (sc-char s)))
      (sc-error s (if (eq kind :anchor)
                      "while scanning an anchor"
                      "while scanning an alias")
                start "found a flow indicator in an anchor or alias"))
    (unless (plusp (fill-pointer b))
      (sc-error s (if (eq kind :anchor)
                      "while scanning an anchor"
                      "while scanning an alias")
                start "did not find expected anchor name"))
    (make-token kind start (sc-mark s) :value (scan-buffer-string b))))

(defun scan-tag (s)
  (let ((start (sc-mark s)) handle suffix)
    (if (sc-check s #\< 1)
        (progn (setf handle "") (sc-skip s) (sc-skip s)
               (setf suffix (scan-tag-uri s t nil nil start))
               (unless (sc-check s #\>) (sc-error s "while scanning a tag" start
                                                   "did not find the expected '>'"))
               (sc-skip s))
        (progn
          (setf handle (scan-tag-handle s nil start))
          (if (and (> (length handle) 1)
                   (char= (char handle (1- (length handle))) #\!))
              (setf suffix (scan-tag-uri s nil nil nil start))
              (progn (setf suffix (scan-tag-uri s nil nil handle start) handle "!")
                     (when (zerop (length suffix)) (rotatef handle suffix))))))
    (unless (or (sc-blankz-p s) (and (> (scanner-flow-level s) 0) (sc-check s #\,)))
      (sc-error s "while scanning a tag" start "did not find expected whitespace or line break"))
    (make-token :tag start (sc-mark s) :handle handle :suffix suffix)))

(defun scan-tag-handle (s directive start)
  (let ((b (make-scan-buffer)))
    (unless (sc-check s #\!) (sc-error s (if directive "while scanning a tag directive"
                                             "while scanning a tag") start "did not find expected '!'"))
    (sc-read s b) (loop while (sc-alpha-p s) do (sc-read s b))
    (if (sc-check s #\!) (sc-read s b)
        (when (and directive (> (fill-pointer b) 1))
          (sc-error s "while parsing a tag directive" start "did not find expected '!'")))
    (scan-buffer-string b)))

(defun scan-tag-uri (s uri-char directive head start)
  (let ((b (make-scan-buffer)) (length (if head (length head) 0)))
    (when head (loop for i from 1 below (length head) do (vector-push-extend (char head i) b)))
    (loop while (or (sc-alpha-p s) (find (sc-char s) ";/?:@&=+$.!~*'()%_" :test #'char=)
                    (and uri-char (find (sc-char s) ",[]" :test #'char=))) do
      (if (and (sc-check s #\%) (sc-hex-p s 1) (sc-hex-p s 2)
               (< (+ (* 16 (sc-hex-value s 1)) (sc-hex-value s 2)) #x80))
          (progn
            (vector-push-extend
             (code-char (+ (* 16 (sc-hex-value s 1)) (sc-hex-value s 2))) b)
            (dotimes (i 3) (sc-skip s)))
          (if (sc-check s #\%) (scan-uri-escapes s directive start b) (sc-read s b)))
      (incf length))
    (when (zerop length) (sc-error s (if directive "while parsing a %TAG directive"
                                        "while parsing a tag") start "did not find expected tag URI"))
    (scan-buffer-string b)))
