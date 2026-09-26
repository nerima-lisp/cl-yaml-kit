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
                        (explicit-document-start nil))
  "Return YAML text representing VALUE. NIL is represented as the null scalar."
  (let ((root (represent value)))
    (labels ((apply-style (node)
               (when (or (sequence-node-p node) (mapping-node-p node))
                 (setf (node-style node) default-flow-style)
                 (dolist (child (%node-children node)) (apply-style child)))) )
      (apply-style root))
    (with-output-to-string (stream)
      (let ((events nil))
        (serialize root (lambda (event) (push event events))
                   :explicit-document-start explicit-document-start)
        (emit-event-stream (nreverse events) stream :indent indent :width width
                           :explicit-document-start explicit-document-start)))))

(defun write-yaml (value stream &key (indent 2) (width 80)
                                    (default-flow-style :block)
                                    (explicit-document-start nil))
  "Write YAML representing VALUE to STREAM and return VALUE."
  (write-string (emit value :indent indent :width width
                      :default-flow-style default-flow-style
                      :explicit-document-start explicit-document-start)
                stream)
  value)
