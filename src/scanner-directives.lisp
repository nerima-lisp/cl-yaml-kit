(in-package #:yaml-kit)
(declaim (optimize (speed 3) (safety 1)))
(defconstant +max-version-number-length+ 9)

(defun scan-directive (s)
  "yaml_parser_scan_directive."
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
             (make-token :version-directive start end :major major :minor minor))))
        ((string= name "TAG")
         (multiple-value-bind (handle prefix) (scan-tag-directive-value s start)
           (let ((end (sc-mark s))) (scan-directive-end s start)
             (make-token :tag-directive start end :handle handle :value prefix))))
        (t
         ;; YAML 1.2.2 reserves unknown directives; consume their line.
         (loop until (sc-breakz-p s) do (sc-skip s))
         (when (sc-break-p s) (sc-skip-line s))
         ;; Comments and blank lines between a reserved directive and the
         ;; document marker are still part of the scanner's inter-token gap.
         (loop
           (loop while (sc-blank-p s) do (sc-skip s))
           (cond
             ((sc-break-p s) (sc-skip-line s))
             ((sc-check s #\#)
              (loop until (sc-breakz-p s) do (sc-skip s))
              (when (sc-break-p s) (sc-skip-line s)))
             (t (return))))
         (cond
           ((sc-check s #\%) (scan-directive s))
           ;; A directive must be followed by a document start marker.  The
           ;; fetcher already has a token slot reserved for this directive, so
           ;; return the marker here after ignoring the reserved directive.
           ((and (sc-check s #\-) (sc-check s #\- 1) (sc-check s #\- 2)
                 (sc-blankz-p s 3))
            (let ((document-start (sc-mark s)))
              (dotimes (i 3) (sc-skip s))
              (make-token :document-start document-start (sc-mark s))))
           (t
            (sc-error s "while scanning a directive" start
                      "found reserved directive name"))))))))

(defun scan-directive-end (s start)
  (loop while (sc-blank-p s) do (sc-skip s))
  (when (sc-check s #\#) (loop until (sc-breakz-p s) do (sc-skip s)))
  (unless (sc-breakz-p s) (sc-error s "while scanning a directive" start
                                      "did not find expected comment or line break"))
  (when (sc-break-p s) (sc-skip-line s)))

(defun scan-directive-name (s start)
  "yaml_parser_scan_directive_name."
  (let ((b (make-scan-buffer)))
    (loop while (sc-alpha-p s) do (sc-read s b))
    (when (zerop (fill-pointer b)) (sc-error s "while scanning a directive" start
                                               "could not find expected directive name"))
    (unless (sc-blankz-p s) (sc-error s "while scanning a directive" start
                                      "found unexpected non-alphabetical character"))
    (scan-buffer-string b)))

(defun scan-version-directive-value (s start)
  "yaml_parser_scan_version_directive_value."
  (loop while (sc-blank-p s) do (sc-skip s))
  (let ((major (scan-version-directive-number s start)))
    (unless (sc-check s #\.) (sc-error s "while scanning a %YAML directive" start
                                        "did not find expected digit or '.' character"))
    (sc-skip s) (values major (scan-version-directive-number s start))))

(defun scan-version-directive-number (s start)
  "yaml_parser_scan_version_directive_number."
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
  "yaml_parser_scan_tag_directive_value."
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
  "yaml_parser_scan_anchor."
  (let ((start (sc-mark s)) (b (make-scan-buffer)))
    (sc-skip s)
    ;; ns-anchor-char is ns-char minus c-flow-indicator.  A colon or
    ;; question mark followed by separation starts the surrounding mapping
    ;; syntax, so retain the scanner's existing anchor boundary there.
    (loop while (and (sc-printable-p s)
                     (not (sc-bom-p s))
                     (not (sc-blankz-p s))
                     (not (yaml-flow-indicator-p (sc-char s)))
                     (not (and (member (sc-char s) '(#\? #\:))
                               (sc-blankz-p s 1))))
          do (sc-read s b))
    (unless (plusp (fill-pointer b))
      (sc-error s (if (eq kind :anchor)
                      "while scanning an anchor"
                      "while scanning an alias")
                start "did not find expected anchor name"))
    (make-token kind start (sc-mark s) :value (scan-buffer-string b))))

(defun scan-tag (s)
  "yaml_parser_scan_tag."
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
  "yaml_parser_scan_tag_handle."
  (let ((b (make-scan-buffer)))
    (unless (sc-check s #\!) (sc-error s (if directive "while scanning a tag directive"
                                             "while scanning a tag") start "did not find expected '!'"))
    (sc-read s b) (loop while (sc-alpha-p s) do (sc-read s b))
    (if (sc-check s #\!) (sc-read s b)
        (when (and directive (> (fill-pointer b) 1))
          (sc-error s "while parsing a tag directive" start "did not find expected '!'")))
    (scan-buffer-string b)))

(defun scan-tag-uri (s uri-char directive head start)
  "yaml_parser_scan_tag_uri."
  (let ((b (make-scan-buffer)) (length (if head (length head) 0)))
    (when head (loop for i from 1 below (length head) do (vector-push-extend (char head i) b)))
    (loop while (or (sc-alpha-p s) (find (sc-char s) ";/?:@&=+$.!~*'()%" :test #'char=)
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

(defun scan-uri-escapes (s directive start b)
  "yaml_parser_scan_uri_escapes."
  (let ((width 0) (octets (make-array 4 :element-type '(unsigned-byte 8)
                                      :adjustable t :fill-pointer 0)))
    (loop while (or (zerop (fill-pointer octets)) (plusp width)) do
      (unless (and (sc-check s #\%) (sc-hex-p s 1) (sc-hex-p s 2))
        (sc-error s (if directive "while parsing a %TAG directive" "while parsing a tag") start
                  "did not find URI escaped octet"))
      (let ((octet (+ (* 16 (sc-hex-value s 1)) (sc-hex-value s 2))))
        (unless (plusp width) (setf width (cond ((zerop (logand octet #x80)) 1)
                                                ((= (logand octet #xe0) #xc0) 2)
                                                ((= (logand octet #xf0) #xe0) 3)
                                                ((= (logand octet #xf8) #xf0) 4)
                                                (t 0)))
        (when (zerop width) (sc-error s "while parsing a tag" start
                                      "found an incorrect leading UTF-8 octet"))
        (when (and (plusp (fill-pointer octets)) (/= (logand octet #xc0) #x80))
          (sc-error s "while parsing a tag" start "found an incorrect trailing UTF-8 octet"))
        (vector-push-extend octet octets) (dotimes (i 3) (sc-skip s)))
      (decf width)
      (when (zerop width) (return)))
    (let ((code (case (fill-pointer octets)
                  (1 (aref octets 0))
                  (2 (+ (ash (logand (aref octets 0) #x1f) 6) (logand (aref octets 1) #x3f)))
                  (3 (+ (ash (logand (aref octets 0) #xf) 12) (ash (logand (aref octets 1) #x3f) 6)
                        (logand (aref octets 2) #x3f)))
                  (otherwise (+ (ash (logand (aref octets 0) #x7) 18)
                                (ash (logand (aref octets 1) #x3f) 12)
                                (ash (logand (aref octets 2) #x3f) 6)
                                (logand (aref octets 3) #x3f))))))
      (vector-push-extend (code-char code) b)))))
