(in-package #:yaml-kit)

(defstruct (format-edit-node (:constructor %make-format-edit-node)
                             (:copier nil))
  kind
  start
  end
  anchor
  tag
  style
  value
  children
  pairs)

(defstruct (format-edit-pair (:constructor %make-format-edit-pair)
                             (:copier nil))
  key
  value)

(defun %format-edit-event-offset (event accessor)
  (let ((mark (funcall accessor event)))
    (unless mark
      (%format-edit-structure-error nil "event has no source mark"))
    (mark-offset mark)))

(defun %format-edit-node-from-events (events)
  (let ((events (coerce events 'vector)))
    (labels ((parse-node (index)
               (let ((event (aref events index)))
                 (cond
                   ((scalar-event-p event)
                    (values (%make-format-edit-node
                             :kind :scalar
                             :start (%format-edit-event-offset event #'event-start-mark)
                             :end (%format-edit-event-offset event #'event-end-mark)
                             :anchor (scalar-event-anchor event)
                             :tag (scalar-event-tag event)
                             :style (scalar-event-style event)
                             :value (scalar-event-value event))
                            (1+ index)))
                   ((alias-event-p event)
                    (values (%make-format-edit-node
                             :kind :alias
                             :start (%format-edit-event-offset event #'event-start-mark)
                             :end (%format-edit-event-offset event #'event-end-mark)
                             :anchor (alias-event-anchor event))
                            (1+ index)))
                   ((sequence-start-event-p event)
                    (let ((children nil)
                          (next (1+ index)))
                      (loop until (sequence-end-event-p (aref events next))
                            do (multiple-value-bind (child new-next) (parse-node next)
                                 (push child children)
                                 (setf next new-next)))
                      (let ((end-event (aref events next)))
                        (values (%make-format-edit-node
                                 :kind :sequence
                                 :start (%format-edit-event-offset event #'event-start-mark)
                                 :end (%format-edit-event-offset end-event #'event-end-mark)
                                 :anchor (sequence-start-event-anchor event)
                                 :tag (sequence-start-event-tag event)
                                 :style (sequence-start-event-style event)
                                 :children (nreverse children))
                                (1+ next)))))
                   ((mapping-start-event-p event)
                    (let ((pairs nil)
                          (next (1+ index)))
                      (loop until (mapping-end-event-p (aref events next))
                            do (multiple-value-bind (key after-key) (parse-node next)
                                 (multiple-value-bind (value after-value) (parse-node after-key)
                                   (push (%make-format-edit-pair :key key :value value) pairs)
                                   (setf next after-value))))
                      (let ((end-event (aref events next)))
                        (values (%make-format-edit-node
                                 :kind :mapping
                                 :start (%format-edit-event-offset event #'event-start-mark)
                                 :end (%format-edit-event-offset end-event #'event-end-mark)
                                 :anchor (mapping-start-event-anchor event)
                                 :tag (mapping-start-event-tag event)
                                 :style (mapping-start-event-style event)
                                 :pairs (nreverse pairs))
                                (1+ next)))))
                   (t
                    (%format-edit-structure-error nil "unexpected event while building edit tree"))))))
      (let ((index 0)
            (roots nil))
        (unless (and (< index (length events))
                     (stream-start-event-p (aref events index)))
          (%format-edit-structure-error nil "event stream has no stream start"))
        (incf index)
        (loop until (and (< index (length events))
                         (stream-end-event-p (aref events index)))
              do (unless (and (< index (length events))
                              (document-start-event-p (aref events index)))
                   (%format-edit-structure-error nil "event stream has an invalid document boundary"))
                 (incf index)
                 (multiple-value-bind (root after-root) (parse-node index)
                   (push root roots)
                   (setf index after-root))
                 (unless (and (< index (length events))
                              (document-end-event-p (aref events index)))
                   (%format-edit-structure-error nil "document has no end event"))
                 (incf index))
        (unless (< index (length events))
          (%format-edit-structure-error nil "event stream has no stream end"))
        (nreverse roots)))))

(defun %format-edit-map-pair (node key)
  (unless (stringp key)
    (return-from %format-edit-map-pair nil))
  (find-if (lambda (pair)
             (let ((key-node (format-edit-pair-key pair)))
               (and (eq (format-edit-node-kind key-node) :scalar)
                    (string= key
                             (format-edit-node-value key-node)))))
           (format-edit-node-pairs node)))

(defun %format-edit-map-duplicate-key-p (node)
  (let ((keys nil))
    (some (lambda (pair)
            (let ((key (format-edit-pair-key pair)))
              (and (eq (format-edit-node-kind key) :scalar)
                   (if (member (format-edit-node-value key) keys :test #'string=)
                       t
                       (progn
                         (push (format-edit-node-value key) keys)
                         nil)))))
          (format-edit-node-pairs node))))

(defun %format-edit-map-merge-key-p (node)
  (some (lambda (pair)
          (let ((key (format-edit-pair-key pair)))
            (and (eq (format-edit-node-kind key) :scalar)
                 (string= (format-edit-node-value key) "<<"))))
        (format-edit-node-pairs node)))

(defun %format-edit-inline-sequence-node-p (node text)
  (when (member (format-edit-node-kind node) '(:mapping :sequence))
    (let* ((start (format-edit-node-start node))
           (line-start (%format-edit-line-start text start))
           (prefix (subseq text line-start start))
           (dash (position #\- prefix :from-end t)))
      (and dash
           (every (lambda (character)
                    (member character '(#\Space #\Tab)))
                   (subseq prefix (1+ dash)))))))

(defun %format-edit-only-child-p (node)
  (case (format-edit-node-kind node)
    (:mapping (= (length (format-edit-node-pairs node)) 1))
    (:sequence (= (length (format-edit-node-children node)) 1))
    (otherwise nil)))

(defun %format-edit-scalar-span-single-line-p (text node)
  (let ((start (format-edit-node-start node))
        (end (format-edit-node-end node)))
    (and (< start end)
         (not (find-if (lambda (character)
                         (member character '(#\Return #\Newline)))
                       (subseq text start end))))))

(defun %format-edit-check-set-node (text node path)
  (unless (eq (format-edit-node-kind node) :scalar)
    (%format-edit-structure-error path
                                  "setting collection values is not supported"))
  (unless (member (format-edit-node-style node)
                  '(:plain :single-quoted :double-quoted))
    (%format-edit-structure-error path
                                  "setting block or empty scalar values is not supported"))
  (unless (%format-edit-scalar-span-single-line-p text node)
    (%format-edit-structure-error path
                                  "setting multiline or empty scalar values is not supported")))

(defun %format-edit-node-anchored-p (node)
  (or (format-edit-node-anchor node)
      (eq (format-edit-node-kind node) :alias)))

(defun %format-edit-check-safe-node (node path)
  (when (format-edit-node-tag node)
    (%format-edit-structure-error path
                                  "edits involving tagged nodes are not supported"))
  (when (%format-edit-node-anchored-p node)
    (%format-edit-anchor-error path (format-edit-node-anchor node)
                               "anchor or alias is involved")))

(defun %format-edit-find (root path document operation)
  (let ((node root) (parent nil) (component nil))
    (dolist (part path)
      (when (format-edit-node-tag node)
        (%format-edit-structure-error path
                                      "edits through tagged nodes are not supported"))
      (when (%format-edit-node-anchored-p node)
        (%format-edit-anchor-error path (format-edit-node-anchor node)
                                   "path crosses an anchor or alias"))
      (case (format-edit-node-kind node)
        (:mapping
         (unless (stringp part)
           (%format-edit-path-error path document operation
                               "mapping path components must be strings"))
         (when (%format-edit-map-duplicate-key-p node)
           (%format-edit-structure-error path "duplicate mapping keys are not supported"))
         (when (%format-edit-map-merge-key-p node)
           (%format-edit-structure-error path "merge-key mappings are not supported"))
         (let ((pair (%format-edit-map-pair node part)))
           (unless pair (return-from %format-edit-find
                          (values nil node part)))
           (let ((key (format-edit-pair-key pair)))
             (when (%format-edit-node-anchored-p key)
               (%format-edit-anchor-error path (format-edit-node-anchor key)
                                          "mapping key has an anchor or alias")))
           (setf parent node
                 component part
                 node (format-edit-pair-value pair))))
        (:sequence
         (unless (and (integerp part) (<= 0 part)
                      (< part (length (format-edit-node-children node))))
           (return-from %format-edit-find
             (values nil node part)))
         (setf parent node
               component part
               node (nth part (format-edit-node-children node))))
        (otherwise
         (return-from %format-edit-find
           (values nil node component)))))
    (values node parent component)))
