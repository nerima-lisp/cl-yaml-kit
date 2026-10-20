(in-package #:yaml-kit)

(defun %format-edit-single-quoted-fragment (value)
  (with-output-to-string (stream)
    (write-char #\' stream)
    (loop for character across value
          do (when (char= character #\')
               (write-char #\' stream))
             (write-char character stream))
    (write-char #\' stream)))

(defun %format-edit-double-quoted-fragment (value)
  (with-output-to-string (stream)
    (write-char #\" stream)
    (loop for character across value
          for code = (char-code character)
          do (let ((escape (gethash character *yaml-escape-table*)))
               (if escape
                   (write-string escape stream)
                   (if (%yaml-printable-character-p character)
                       (write-char character stream)
                       (if (<= code #xff)
                           (format stream "\\x~2,'0X" code)
                           (if (<= code #xffff)
                               (format stream "\\u~4,'0X" code)
                               (format stream "\\U~8,'0X" code)))))))
    (write-char #\" stream)))

(defun %format-edit-flow-dangerous-string-p (value)
  (or (zerop (length value))
      (some (lambda (character)
              (member character '(#\Return #\Newline #\[ #\] #\{ #\} #\,)))
            value)
      (loop for index below (length value)
            thereis (and (char= (char value index) #\:)
                         (< (1+ index) (length value))
                         (or (member (char value (1+ index))
                                     '(#\Space #\Tab #\Return #\Newline
                                       #\[ #\] #\{ #\} #\,))
                             (and (< (+ index 2) (length value))
                                  (char= (char value (1+ index)) #\?)))))))

(defun %format-edit-fragment (value &optional flow-p)
  (when (or (consp value)
            (yaml-mapping-p value)
            (and (arrayp value) (not (stringp value)))
            (hash-table-p value))
    (%format-edit-structure-error nil "only scalar values can be set"))
  (let ((fragment (string-trim '(#\Return #\Newline #\Space #\Tab)
                               (emit value :default-flow-style :flow))))
    (if (and (stringp value)
             (find-if (lambda (character)
                        (member character '(#\Return #\Newline)))
                      fragment))
        (%format-edit-double-quoted-fragment value)
        (if (and flow-p (stringp value)
                 (%format-edit-flow-dangerous-string-p value))
            (%format-edit-single-quoted-fragment value)
            fragment))))

(defun %format-edit-insertion-position (node text)
  (let ((position (format-edit-node-end node)))
    (unless (<= 0 position (length text))
      (%format-edit-structure-error nil "node end is outside source"))
    position))

(defun %format-edit-indent (node text)
  (let* ((start (format-edit-node-start node))
         (line-start (%format-edit-line-start text start)))
    (when (and (= line-start 0)
               (< line-start (length text))
               (char= (char text line-start) (code-char #xFEFF)))
      (incf line-start))
    (subseq text line-start start)))

(defun %format-edit-apply-replacement (text start end replacement)
  (concatenate 'simple-string (subseq text 0 start) replacement (subseq text end)))

(defun %format-edit-encode (encoder text)
  (if encoder (funcall encoder text) text))

(defun %format-edit-preserve-final-newline (original edited)
  (if (and (plusp (length original))
           (member (char original (1- (length original))) '(#\Return #\Newline))
           (plusp (length edited))
           (not (member (char edited (1- (length edited)))
                        '(#\Return #\Newline))))
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

(defun %format-edit-set-missing (text parent component value path document)
  (when (eq (format-edit-node-style parent) :flow)
    (%format-edit-structure-error path "adding to a flow collection is not supported"))
  (when (%format-edit-inline-sequence-node-p parent text)
    (%format-edit-structure-error path
                                  "adding to sequence-inline nodes is not supported"))
  (let ((fragment (%format-edit-fragment value))
        (newline (%format-edit-newline text))
        (indent (%format-edit-indent parent text))
        (position (%format-edit-insertion-position parent text)))
    (case (format-edit-node-kind parent)
      (:mapping
       (unless (stringp component)
         (%format-edit-path-error path document :set
                             "mapping key must be scalar"))
       (values position
               (concatenate 'simple-string
                            (if (and (plusp position)
                                     (not (member (char text (1- position))
                                                  '(#\Return #\Newline))))
                                newline "")
                            indent (%format-edit-fragment component) ": " fragment
                            (if (< position (length text)) newline ""))))
      (:sequence
       (unless (and (integerp component) (<= 0 component))
         (%format-edit-path-error path document :set
                             "sequence edits only append at the next index"))
       (unless (= component (length (format-edit-node-children parent)))
         (%format-edit-structure-error path
                                       "sequence additions must append at the next index"))
       (values position
               (concatenate 'simple-string
                            (if (and (plusp position)
                                     (not (member (char text (1- position))
                                                  '(#\Return #\Newline))))
                                newline "")
                            indent "- " fragment
                            (if (< position (length text)) newline ""))))
      (otherwise
       (%format-edit-path-error path document :set
                           "path parent is not a collection")))))
