(in-package #:yaml-kit)

(defun %format-edit-single-quoted-fragment (value)
  (with-output-to-string (stream)
    (write-char *format-edit-single-quote-character* stream)
    (loop for character across value
          do (when (char= character *format-edit-single-quote-character*)
               (write-char *format-edit-single-quote-character* stream))
             (write-char character stream))
    (write-char *format-edit-single-quote-character* stream)))

(defun %format-edit-double-quoted-fragment (value)
  (with-output-to-string (stream)
    (write-char *format-edit-double-quote-character* stream)
    (loop for character across value
          for code = (char-code character)
          do (let ((escape (gethash character *yaml-escape-table*)))
               (if escape
                   (write-string escape stream)
                   (if (yaml-printable-p character)
                       (write-char character stream)
                       (if (<= code #xff)
                           (format stream "\\x~2,'0X" code)
                           (if (<= code #xffff)
                               (format stream "\\u~4,'0X" code)
                               (format stream "\\U~8,'0X" code)))))))
    (write-char *format-edit-double-quote-character* stream)))

(defun %format-edit-flow-dangerous-string-p (value)
  (or (zerop (length value))
      (some (lambda (character)
              (or (yaml-line-break-p character)
                  (yaml-flow-indicator-p character)))
            value)
      (loop for index below (length value)
            for next-index = (1+ index)
            thereis (and (char= (char value index)
                                *format-edit-colon-character*)
                         (< next-index (length value))
                         (or (yaml-whitespace-p (char value next-index))
                             (yaml-line-break-p (char value next-index))
                             (yaml-flow-indicator-p (char value next-index))
                             (and (< (+ index 2) (length value))
                                  (char= (char value next-index)
                                         *format-edit-question-character*)))))))

(progn
  (defun %format-edit-layout-whitespace-p (character)
    (or (yaml-whitespace-p character)
        (yaml-line-break-p character)))

  (defun %format-edit-trim-layout (text)
    (let ((start (position-if-not #'%format-edit-layout-whitespace-p text))
          (end (position-if-not #'%format-edit-layout-whitespace-p
                                text :from-end t)))
      (if start
          (subseq text start (1+ end))
          "")))

  (defun %format-edit-check-printable-value (value)
    (when (and (stringp value)
               (find-if (lambda (character)
                          (let ((code (char-code character)))
                            (and (not (yaml-printable-p character))
                                 (<= #xfffe code #xffff))))
                        value))
      (%format-edit-structure-error
       nil "U+FFFE and U+FFFF are not allowed in YAML scalar values"))))

(defun %format-edit-fragment (value &optional flow-p)
  (when (or (consp value)
            (yaml-mapping-p value)
            (and (arrayp value) (not (stringp value)))
            (hash-table-p value))
    (%format-edit-structure-error nil "only scalar values can be set"))
  (%format-edit-check-printable-value value)
  (let ((fragment
          (%format-edit-trim-layout
           (emit value :default-flow-style :flow))))
    (if (and (stringp value)
             (find-if #'yaml-line-break-p fragment))
        (%format-edit-double-quoted-fragment value)
        (if (and flow-p (stringp value)
                 (%format-edit-flow-dangerous-string-p value))
            (%format-edit-single-quoted-fragment value)
            fragment))))

(defun %format-edit-insertion-position (node text)
  (let* ((pairs (format-edit-node-pairs node))
         (last-pair (and pairs (car (last pairs))))
         (position
           (if last-pair
               (%format-edit-after-newline
                text
                (%format-edit-line-end
                 text
                 (format-edit-node-end
                  (format-edit-pair-value last-pair))))
               (format-edit-node-end node))))
    (unless (<= 0 position (length text))
      (%format-edit-structure-error nil "node end is outside source"))
    position))

(defun %format-edit-indent (node text)
  (let* ((start (format-edit-node-start node))
         (line-start (%format-edit-line-start text start)))
    (when (and (= line-start 0)
               (< line-start (length text))
               (char= (char text line-start) +format-edit-bom-character+))
      (incf line-start))
    (subseq text line-start start)))

(defun %format-edit-apply-replacement (text start end replacement)
  (concatenate 'simple-string (subseq text 0 start) replacement (subseq text end)))

(defun %format-edit-encode (encoder text)
  (if encoder (funcall encoder text) text))

(defun %format-edit-preserve-final-newline (original edited)
  (if (and (plusp (length original))
           (yaml-line-break-p (char original (1- (length original))))
           (plusp (length edited))
           (not (yaml-line-break-p (char edited (1- (length edited))))))
      (concatenate 'simple-string edited (%format-edit-newline original))
      edited))

(defun %format-edit-validate-text (text path)
  (handler-case
      (values text (parse-all text))
    (yaml-kit-error (condition)
      (%format-edit-structure-error
       path (format nil "edit produced invalid YAML: ~A" condition)))))

(defun %format-edit-delete-entry (text parent pair-or-node path)
  (when (eq (format-edit-node-style parent) :flow)
    (%format-edit-structure-error path "deleting from a flow collection is not supported"))
  (let ((start (if (format-edit-pair-p pair-or-node)
                   (format-edit-node-start (format-edit-pair-key pair-or-node))
                   (format-edit-node-start pair-or-node)))
        (end (if (format-edit-pair-p pair-or-node)
                 (format-edit-node-end (format-edit-pair-value pair-or-node))
                 (format-edit-node-end pair-or-node))))
    (%format-edit-block-span text start end)))

(defun %format-edit-insertion-text (text position newline indent prefix fragment)
  (concatenate
   'simple-string
   (if (and (plusp position)
            (not (yaml-line-break-p (char text (1- position)))))
       newline "")
   indent prefix fragment
   (if (< position (length text)) newline "")))

(defun %format-edit-mapping-insertion
    (text position component fragment newline indent path document)
  (unless (stringp component)
    (%format-edit-path-error path document :set "mapping key must be scalar"))
  (values position
          (%format-edit-insertion-text
           text position newline indent
           (concatenate 'simple-string (%format-edit-fragment component) ": ")
           fragment)))

(defun %format-edit-sequence-insertion
    (text parent position component fragment newline indent path document)
  (unless (and (integerp component) (<= 0 component))
    (%format-edit-path-error
     path document :set "sequence edits only append at the next index"))
  (unless (%format-edit-sequence-index-p
           component (length (format-edit-node-children parent)) t)
    (%format-edit-structure-error
     path "sequence additions must append at the next index"))
  (values position
          (%format-edit-insertion-text
           text position newline indent "- " fragment)))

(defun %format-edit-set-missing (text parent component value path document)
  (when (eq (format-edit-node-style parent) :flow)
    (%format-edit-structure-error path
                                  "adding to a flow collection is not supported"))
  (when (%format-edit-inline-sequence-node-p parent text)
    (%format-edit-structure-error path
                                  "adding to sequence-inline nodes is not supported"))
  (let ((fragment (%format-edit-fragment value))
        (newline (%format-edit-newline text))
        (indent (%format-edit-indent parent text))
        (position (%format-edit-insertion-position parent text)))
    (case (format-edit-node-kind parent)
      (:mapping
       (%format-edit-mapping-insertion
        text position component fragment newline indent path document))
      (:sequence
       (%format-edit-sequence-insertion
        text parent position component fragment newline indent path document))
      (otherwise
       (%format-edit-path-error
        path document :set "path parent is not a collection")))))
