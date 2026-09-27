(in-package #:yaml-kit)

(declaim (inline %limit!))

(defun event-list-source (events)
  (lambda (handler)
    (dolist (event events)
      (funcall handler event))))

(defun %limit! (value limit &optional (name "resource"))
  (when (and limit (> value limit))
    (error 'yaml-resource-limit-error
           :limit-name name :limit limit :actual value)))

(defun compose-all-events (events &key (max-input-length +default-max-input-length+)
                                     (max-depth +default-max-depth+)
                                     (max-scalar-length +default-max-scalar-length+)
                                     (max-nodes +default-max-nodes+)
                                     (max-alias-expansions +default-max-alias-expansions+)
                                     first-only document-handler)
  "Compose EVENTS without retaining an intermediate event list."
  (declare (optimize (speed 3) (safety 1))
           (ignore max-input-length))
  (let ((documents nil) (current nil) (stack nil) (anchors nil)
        (nodes 0) (depth 0) (aliases 0))
    (labels
        ((append-node (node)
           (if stack
               (push node (cdr (car stack)))
               (if current
                   (signal-yaml-compose-error :cause "multiple root nodes")
                   (setf current node))))
        (start-document ()
           (setf current nil stack nil depth 0
                 anchors (make-hash-table :test #'equal)
                 nodes 0 aliases 0))
         (finish-node ()
           (unless stack (signal-yaml-compose-error :cause "collection end without start"))
           (let* ((frame (pop stack))
                  (node (car frame))
                  (children (nreverse (cdr frame))))
             (if (sequence-node-p node)
                 (setf (sequence-node-items node) children)
                 (progn
                   (unless (evenp (length children))
                     (signal-yaml-compose-error :cause "odd mapping entries"))
                   (setf (mapping-node-pairs node)
                         (loop for (key value) on children by #'cddr
                               collect (cons key value)))))
             (decf depth)
             (if stack
                 (push node (cdr (car stack)))
                 (if current
                     (signal-yaml-compose-error :cause "multiple root nodes")
                     (setf current node)))))
         (start-collection (event sequence-p)
           (let ((node (if sequence-p
                           (make-sequence-node
                            :tag (sequence-start-event-tag event)
                            :anchor (sequence-start-event-anchor event)
                            :style (sequence-start-event-style event)
                            :start-mark (event-start-mark event)
                            :end-mark (event-end-mark event))
                           (make-mapping-node
                            :tag (mapping-start-event-tag event)
                            :anchor (mapping-start-event-anchor event)
                            :style (mapping-start-event-style event)
                            :start-mark (event-start-mark event)
                            :end-mark (event-end-mark event)))))
             (incf nodes)
             (%limit! nodes max-nodes "nodes")
             (when (node-anchor node)
               (setf (gethash (node-anchor node) anchors) node))
             (incf depth)
             (%limit! depth max-depth "depth")
             (push (cons node nil) stack)))
         (handle (event)
           (cond
             ((or (stream-start-event-p event) (stream-end-event-p event)) nil)
             ((document-start-event-p event) (start-document))
             ((document-end-event-p event)
              (when stack (signal-yaml-compose-error :cause "document ended inside collection"))
              (when current
                (if document-handler
                    (funcall document-handler current)
                    (push current documents))
                (when first-only
                  (let ((document current))
                    (setf current nil)
                    (throw 'first-document document))))
              (setf current nil))
             ((alias-event-p event)
              (incf aliases)
              (%limit! aliases max-alias-expansions "alias expansions")
              (multiple-value-bind (node presentp)
                  (gethash (alias-event-anchor event) anchors)
                (unless presentp
                  (signal-yaml-compose-error :mark (event-start-mark event)
                                             :cause "unknown alias"))
                (incf nodes)
                (%limit! nodes max-nodes "nodes")
                (append-node node)))
             ((scalar-event-p event)
              (let ((value (scalar-event-value event)))
                (%limit! (length value) max-scalar-length "scalar")
                (let ((node (make-scalar-node
                           :tag (scalar-event-tag event)
                           :anchor (scalar-event-anchor event)
                           :style (scalar-event-style event)
                           :start-mark (event-start-mark event)
                           :end-mark (event-end-mark event)
                           :value value)))
                  (incf nodes)
                  (%limit! nodes max-nodes "nodes")
                  (when (node-anchor node)
                    (setf (gethash (node-anchor node) anchors) node))
                  (append-node node))))
             ((sequence-start-event-p event) (start-collection event t))
             ((mapping-start-event-p event) (start-collection event nil))
             ((or (sequence-end-event-p event) (mapping-end-event-p event))
              (finish-node))
             (t (signal-yaml-compose-error :cause "unknown event")))))
      (catch 'first-document
        (funcall events #'handle)))
      (when stack (signal-yaml-compose-error :cause "unclosed collection"))
      (when current (push current documents))
      (nreverse documents)))

(defun compose-events (events &rest options)
  "Compose the first graph from an event source."
  (car (apply #'compose-all-events (cons events (append options '(:first-only t))))))
