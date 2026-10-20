(in-package #:cl-yaml-kit/test)

(defun directive-tokens (text)
  (let ((scanner (yaml-kit::make-scanner (scanner-source text)))
        (out nil))
    (loop for token = (yaml-kit::%scanner-next-token scanner)
          while token
          do (push token out))
    (nreverse out)))

(cl-weave:it-each
    ((yaml-version #.(format nil "%YAML 1.2~%")
       ((:version-directive nil nil nil 1 2)))
     (reserved-directive-6lvf
       #.(format nil "%FOO  bar baz # Should be ignored~%--- \"foo\"~%")
       ((:document-start nil nil nil nil nil) (:scalar "foo" nil nil nil nil)))
     (reserved-directive-2lfx
       #.(format nil "%FOO  bar baz # Should be ignored~%---~%\"foo\"~%")
       ((:document-start nil nil nil nil nil) (:scalar "foo" nil nil nil nil)))
     (tag-directive #.(format nil "%TAG !e! tag:example.com,2000:~%")
       ((:tag-directive "tag:example.com,2000:" "!e!" nil nil nil)))
     (tag-handle-and-suffix
       #.(format nil "%TAG !e! tag:example.com,2000:~%--- !e!foo~%")
       ((:tag-directive "tag:example.com,2000:" "!e!" nil nil nil)
        (:document-start nil nil nil nil nil)
        (:tag nil "!e!" "foo" nil nil)))
     (verbatim-tag-keeps-trailing-bang
       #.(format nil "!<tag:yaml.org,2002:str!> value~%")
       ((:tag nil "" "tag:yaml.org,2002:str!" nil nil)
        (:scalar "value" nil nil nil nil)))
     (anchors-and-aliases #.(format nil "&a *a~%")
       ((:anchor "a" nil nil nil nil) (:alias "a" nil nil nil nil)))
     (unicode-anchor #.(format nil "--- &😁 unicode anchor~%")
       ((:document-start nil nil nil nil nil)
        (:anchor "😁" nil nil nil nil)
        (:scalar "unicode anchor" nil nil nil nil)))
     (anchor-accepts-non-flow-punctuation #.(format nil "&a! value~%")
       ((:anchor "a!" nil nil nil nil) (:scalar "value" nil nil nil nil)))
     ;; ns-anchor-char excludes only c-flow-indicator, so ":" stays in the name.
     (anchor-keeps-trailing-colon #.(format nil "&a: value~%")
       ((:anchor "a:" nil nil nil nil) (:scalar "value" nil nil nil nil)))
     (tag-forms #.(format nil "! !!str !e!foo !<tag:yaml.org,2002:str>~%")
       ((:tag nil "" "!" nil nil) (:tag nil "!!" "str" nil nil)
        (:tag nil "!e!" "foo" nil nil)
        (:tag nil "" "tag:yaml.org,2002:str" nil nil)))
     (uri-escape #.(format nil "!%21~%") ((:tag nil "!" "!" nil nil)))
     (uri-escape-utf8 #.(format nil "!%C3%A9%20%E2%82%AC%20%F0%9F%98%81~%")
       ((:tag nil "!" "é € 😁" nil nil)))
     (reserved-directive-comment-gaps
       #.(format nil "%FOO ignored~%~%# first gap~%  # second gap~%%BAR ignored~%---~%")
       ((:document-start nil nil nil nil nil)))
     (recognized-directive-comments-and-eof
       #.(format nil "%YAML 1.2 # version~%%TAG ! tag:example.com,2000: # tag")
       ((:version-directive nil nil nil 1 2)
        (:tag-directive "tag:example.com,2000:" "!" nil nil nil)))
     (primary-tag-handle-carries-suffix
       #.(format nil "!tag:example.com%2C2000:foo~%")
       ((:tag nil "!" "tag:example.com,2000:foo" nil nil)))
     (flow-anchor-stops-at-comma
       #.(format nil "[&a,b]")
       ((:flow-sequence-start nil nil nil nil nil)
        (:anchor "a" nil nil nil nil)
        (:flow-entry nil nil nil nil nil)
        (:scalar "b" nil nil nil nil)
        (:flow-sequence-end nil nil nil nil nil)))
     (verbatim-tag-uri-includes-commas
       #.(format nil "!<tag:example.com,2000:foo> value~%")
       ((:tag nil "" "tag:example.com,2000:foo" nil nil)
        (:scalar "value" nil nil nil nil))))
  "scans directive case ~S"
  (name text expected)
  (declare (ignore name))
  (let* ((tokens (remove-if (lambda (token)
                              (member (yaml-kit:token-kind token)
                                      '(:stream-start :stream-end)))
                            (directive-tokens text)))
         (actual (mapcar (lambda (token)
                           (list (yaml-kit:token-kind token)
                                 (yaml-kit:token-value token)
                                 (yaml-kit:token-handle token)
                                 (yaml-kit:token-suffix token)
                                 (yaml-kit:token-major token)
                                 (yaml-kit:token-minor token)))
                         tokens)))
    (expect actual :to-equal expected)))

(cl-weave:it-each
    ((invalid-version #.(format nil "%YAML 2.0~%"))
     (incomplete-version #.(format nil "%YAML 1.~%"))
     (version-missing-separator #.(format nil "%YAML 1x2~%"))
     (version-missing-number #.(format nil "%YAML .2~%"))
     (version-number-too-long #.(format nil "%YAML 1234567890.1~%"))
     (tag-directive-missing-whitespace
       #.(format nil "%TAG !e!tag:example.com,2000:~%"))
     (tag-uri-invalid-escape #.(format nil "%TAG !e! tag:example.com,%GG~%"))
     (unterminated-verbatim-tag #.(format nil "!<tag:yaml.org,2002:str~%"))
     (empty-anchor #.(format nil "&~%"))
     (empty-alias #.(format nil "*~%"))
     (flow-indicator-in-anchor #.(format nil "&a, value~%"))
     (flow-indicator-in-alias #.(format nil "*a,~%"))
     (uri-escape-overlong-utf8 #.(format nil "!%C0%80~%"))
     (directive-name-unexpected-character #.(format nil "%YAML? 1.2~%"))
     (tag-uri-unexpected-character #.(format nil "%TAG !e! tag:example.com#~%"))
     (tag-handle-missing-leading-bang #.(format nil "%TAG e! tag:example.com~%"))
     (tag-handle-missing-closing-bang #.(format nil "%TAG !e tag:example.com~%"))
     (tag-uri-empty #.(format nil "%TAG !e! ~%"))
     (uri-escape-invalid-leading-octet #.(format nil "!%F8%80%80%80%80~%"))
     (uri-escape-invalid-trailing-octet #.(format nil "!%C3%20~%"))
     (uri-escape-surrogate #.(format nil "!%ED%A0%80~%"))
     (uri-escape-out-of-range #.(format nil "!%F4%90%80%80~%"))
     (uri-escape-truncated #.(format nil "!%E2%82~%"))
     (directive-name-missing #.(format nil "% 1.2~%"))
     (yaml-directive-rejects-trailing-content #.(format nil "%YAML 1.2 extra~%"))
     (tag-directive-rejects-trailing-content
       #.(format nil "%TAG !e! tag:example.com,2000: extra~%"))
     (reserved-directive-requires-document-boundary
       #.(format nil "%FOO ignored~%value~%")))
  "signals a YAML parse error for directive case ~S"
  (name text)
  (declare (ignore name))
  (expect
    (lambda () (scanner-token-kinds text))
    :to-throw
    'yaml-kit:yaml-parse-error))

(cl-weave:it-each
    ((overlong-utf8 "%E0%80%80")
     (surrogate-utf8 "%ED%A0%80")
     (out-of-range-utf8 "%F4%90%80%80")
     (truncated-utf8 "%E2%82")
     (bad-trailing-octet "%C3%20")
     (truncated-four-byte-utf8 "%F0%90"))
  "maps invalid URI escape ~S to a YAML parse error"
  (name text)
  (declare (ignore name))
  (let* ((scanner (yaml-kit::make-scanner (scanner-source text)))
         (buffer (yaml-kit::make-scan-buffer)))
    (expect
      (lambda ()
        (yaml-kit::scan-uri-escapes
         scanner
         nil
         (yaml-kit::sc-position scanner)
         buffer))
      :to-throw
      'yaml-kit:yaml-parse-error)))

(it "stops anchor names before a byte order mark"
  (let* ((scanner (yaml-kit::make-scanner (format nil "&a~C" #\UFEFF)))
         (token (yaml-kit::scan-anchor scanner :anchor)))
    (expect (list (yaml-kit:token-kind token)
                  (yaml-kit:token-value token)
                  (yaml-kit::scanner-pos scanner))
            :to-equal '(:anchor "a" 2))))
