;;;; src/dumper.lisp
(in-package #:yaml-kit)

(defun emit-events (events &key (indent 2) (width 80)
                                  (explicit-document-start nil))
  "Return YAML text for an event list or event-producing function."
  (with-output-to-string (stream)
    (let ((items (if (functionp events)
                     (let ((result nil))
                       (funcall events (lambda (event) (push event result)))
                       (nreverse result))
                     events)))
      (emit-event-stream items stream :indent indent :width width
                         :explicit-document-start explicit-document-start))))

(defun emit (value &key (indent 2) (width 80) (default-flow-style :block)
                        (explicit-document-start nil) canonical)
  "Return YAML text representing VALUE. NIL is an empty sequence; +YAML-NULL+ is null.
CANONICAL is reserved for canonical marker selection."
  (declare (ignore canonical))
  (let ((root (represent value)))
    (let ((seen (make-hash-table :test #'eq)))
      (labels ((apply-style (node)
                 (when (and (or (sequence-node-p node) (mapping-node-p node))
                            (not (gethash node seen)))
                   (setf (gethash node seen) t
                         (node-style node)
                         (if (and (eq default-flow-style :block)
                                  (or (and (sequence-node-p node)
                                           (null (sequence-node-items node)))
                                      (and (mapping-node-p node)
                                           (null (mapping-node-pairs node)))))
                             :flow
                             default-flow-style))
                   (dolist (child (%node-children node)) (apply-style child)))))
        (apply-style root))
      (with-output-to-string (stream)
        (let ((events nil))
          (serialize root (lambda (event) (push event events))
                     :explicit-document-start explicit-document-start)
          (emit-event-stream (nreverse events) stream :indent indent :width width
                             :explicit-document-start explicit-document-start))))))

(defun write-yaml (value stream &key (indent 2) (width 80)
                                    (default-flow-style :block)
                                    (explicit-document-start nil) canonical)
  "Write YAML representing VALUE to STREAM and return VALUE."
  (write-string (emit value :indent indent :width width
                      :default-flow-style default-flow-style
                      :explicit-document-start explicit-document-start
                      :canonical canonical)
                stream)
  value)
