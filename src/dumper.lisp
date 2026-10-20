;;;; src/dumper.lisp
(in-package #:yaml-kit)

(defun emit-events (events &key (indent 2)
                                  (explicit-document-start nil))
  "Return YAML text for an event list."
  (emit-events/k events #'identity
                 :indent indent
                 :explicit-document-start explicit-document-start))

(defun emit (value &key (indent 2) (default-flow-style :block)
                        (explicit-document-start nil))
  "Return YAML text representing VALUE. NIL is an empty sequence; +YAML-NULL+ is null."
  (emit/k value #'identity
          :indent indent
          :default-flow-style default-flow-style
          :explicit-document-start explicit-document-start))

(defun write-yaml (value stream &key (indent 2)
                                    (default-flow-style :block)
                                    (explicit-document-start nil))
  "Write YAML representing VALUE to STREAM and return VALUE."
  (write-yaml/k value stream #'identity
                :indent indent
                :default-flow-style default-flow-style
                :explicit-document-start explicit-document-start))
(defun write-yaml/k (value stream k &key (indent 2)
                                      (default-flow-style :block)
                                      (explicit-document-start nil))
  (unless (functionp k)
    (error 'type-error :datum k :expected-type 'function))
  (emit/k value
          (lambda (output)
            (write-string output stream)
            (values (funcall k value)))
          :indent indent
          :default-flow-style default-flow-style
          :explicit-document-start explicit-document-start))
(defun emit/k (value k &key (indent 2)
                           (default-flow-style :block)
                           (explicit-document-start nil))
  (unless (functionp k)
    (error 'type-error :datum k :expected-type 'function))
  (%representer-represent/cps
   value
   (lambda (root)
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
                    (%walk-node-children node #'apply-style))))
         (apply-style root)))
     (let ((output
             (with-output-to-string (stream)
               (%emitter-emit-event-stream/cps
                (lambda (handler)
                  (%serializer-serialize/cps
                   root handler #'identity
                   :explicit-document-start explicit-document-start))
                stream #'identity
                :indent indent
                :explicit-document-start explicit-document-start
                :suppress-empty-document-marker t))))
       (values (funcall k output))))))
(defun emit-events/k (events k &key (indent 2)
                                  (explicit-document-start nil))
  (unless (functionp k)
    (error 'type-error :datum k :expected-type 'function))
  (let ((output
          (with-output-to-string (stream)
            (%emitter-emit-event-stream/cps
             (event-list-source events) stream #'identity
             :indent indent
             :explicit-document-start explicit-document-start))))
    (values (funcall k output))))
