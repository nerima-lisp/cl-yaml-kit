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
  (let ((span (funcall accessor event)))
    (unless span
      (%format-edit-structure-error nil "event has no source span"))
    (cl-parser-kit:span-start span)))

(defun %format-edit-node-event-p (events index predicate)
  (and (<= 0 index)
       (< index (length events))
       (funcall predicate (aref events index))))

(defun %format-edit-event-node (kind event end-event &optional contents value)
  (%make-format-edit-node
   :kind kind
   :start (%format-edit-event-offset event #'event-start-span)
   :end (%format-edit-event-offset end-event #'event-end-span)
   :anchor (%event-attribute event :anchor)
   :tag (unless (eq kind :alias) (%event-attribute event :tag))
   :style (unless (eq kind :alias) (%event-attribute event :style))
   :value value
   :children (when (eq kind :sequence) contents)
   :pairs (when (eq kind :mapping) contents)))

(defun %format-edit-parse-container-event-node
    (events index kind end-predicate)
  (let ((event (aref events index))
        (next (1+ index))
        (contents nil))
    (loop while (and (< next (length events))
                     (not (funcall end-predicate (aref events next))))
          do (multiple-value-bind (child after-child)
                 (%format-edit-node-from-events-at events next)
               (if (eq kind :mapping)
                   (multiple-value-bind (value after-value)
                       (%format-edit-node-from-events-at events after-child)
                     (push (%make-format-edit-pair :key child :value value)
                           contents)
                     (setf next after-value))
                   (progn
                     (push child contents)
                     (setf next after-child)))))
    (unless (< next (length events))
      (%format-edit-structure-error nil "collection has no end event"))
    (values (%format-edit-event-node
             kind event (aref events next) (nreverse contents))
            (1+ next))))

(defun %format-edit-node-from-events-at (events index)
  (unless (< index (length events))
    (%format-edit-structure-error nil "unexpected end while building edit tree"))
  (let ((event (aref events index)))
    (cond
      ((scalar-event-p event)
       (values (%format-edit-event-node
                :scalar event event nil (scalar-event-value event))
               (1+ index)))
      ((alias-event-p event)
       (values (%format-edit-event-node :alias event event) (1+ index)))
      ((sequence-start-event-p event)
       (%format-edit-parse-container-event-node
        events index :sequence #'sequence-end-event-p))
      ((mapping-start-event-p event)
       (%format-edit-parse-container-event-node
        events index :mapping #'mapping-end-event-p))
      (t
       (%format-edit-structure-error
        nil "unexpected event while building edit tree")))))

(defun %format-edit-node-from-events (events)
  (let* ((events (coerce events 'vector))
         (event-count (length events)))
    (unless (%format-edit-node-event-p events 0 #'stream-start-event-p)
      (%format-edit-structure-error nil "event stream has no stream start"))
    (loop with index = 1
          with roots = nil
          while (and (< index event-count)
                     (not (stream-end-event-p (aref events index))))
          do (unless (%format-edit-node-event-p
                      events index #'document-start-event-p)
               (%format-edit-structure-error
                nil "event stream has an invalid document boundary"))
             (multiple-value-bind (root after-root)
                 (%format-edit-node-from-events-at events (1+ index))
               (push root roots)
               (setf index after-root))
             (unless (%format-edit-node-event-p
                      events index #'document-end-event-p)
               (%format-edit-structure-error nil "document has no end event"))
             (incf index)
          finally
             (unless (< index event-count)
               (%format-edit-structure-error nil "event stream has no stream end"))
             (return (nreverse roots)))))

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
           (dash (position *format-edit-sequence-indicator-character*
                           prefix :from-end t)))
      (and dash
           (every #'yaml-whitespace-p (subseq prefix (1+ dash)))))))

(defun %format-edit-only-child-p (node)
  (case (format-edit-node-kind node)
    (:mapping (= (length (format-edit-node-pairs node)) 1))
    (:sequence (= (length (format-edit-node-children node)) 1))
    (otherwise nil)))

(defun %format-edit-scalar-span-single-line-p (text node)
  (let ((start (format-edit-node-start node))
        (end (format-edit-node-end node)))
    (and (< start end)
         (not (find-if #'yaml-line-break-p (subseq text start end))))))

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

(defun %format-edit-find-map-child (node part path document operation)
  (unless (stringp part)
    (%format-edit-path-error path document operation
                             "mapping path components must be strings"))
  (when (%format-edit-map-duplicate-key-p node)
    (%format-edit-structure-error path "duplicate mapping keys are not supported"))
  (when (%format-edit-map-merge-key-p node)
    (%format-edit-structure-error path "merge-key mappings are not supported"))
  (let ((pair (%format-edit-map-pair node part)))
    (when pair
      (let ((key (format-edit-pair-key pair)))
        (when (%format-edit-node-anchored-p key)
          (%format-edit-anchor-error path (format-edit-node-anchor key)
                                     "mapping key has an anchor or alias")))
      (values (format-edit-pair-value pair) t))))

(defun %format-edit-find (root path document operation)
  (let ((node root) (parent nil) (component nil))
    (dolist (part path)
      (when (format-edit-node-tag node)
        (%format-edit-structure-error
         path "edits through tagged nodes are not supported"))
      (when (%format-edit-node-anchored-p node)
        (%format-edit-anchor-error path (format-edit-node-anchor node)
                                   "path crosses an anchor or alias"))
      (case (format-edit-node-kind node)
        (:mapping
         (multiple-value-bind (child presentp)
             (%format-edit-find-map-child node part path document operation)
           (unless presentp
             (return-from %format-edit-find (values nil node part)))
           (setf parent node component part node child)))
        (:sequence
         (let ((children (format-edit-node-children node)))
           (unless (%format-edit-sequence-index-p part (length children))
             (return-from %format-edit-find (values nil node part)))
           (setf parent node component part node (nth part children))))
        (otherwise
         (return-from %format-edit-find
           (values nil node component)))))
    (values node parent component)))
