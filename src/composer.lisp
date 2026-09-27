(in-package #:yaml-kit)

(defstruct (composer-state (:constructor %make-composer-state))
  documents current stack anchors
  (nodes 0) (depth 0) (aliases 0)
  stream-open-p document-open-p stream-ended-p first-only document-handler)

(defun event-list-source (events)
  (lambda (handler)
    (dolist (event events) (funcall handler event))))

(defun %limit! (value limit &optional (name "resource"))
  (when (and limit (> value limit))
    (error 'yaml-resource-limit-error
           :limit-name name :limit limit :actual value)))

(defun %composer-error (cause &optional mark)
  (signal-yaml-compose-error :mark mark :cause cause))

(defun %composer-start-document (state)
  (unless (and (composer-state-stream-open-p state)
               (not (composer-state-stream-ended-p state))
               (not (composer-state-document-open-p state)))
    (%composer-error "document start outside stream or active document"))
  (setf (composer-state-document-open-p state) t
        (composer-state-current state) nil
        (composer-state-stack state) nil
        (composer-state-anchors state) (make-hash-table :test #'equal)
        (composer-state-nodes state) 0
        (composer-state-depth state) 0
        (composer-state-aliases state) 0))

(defun %composer-append-node (state node)
  (if (composer-state-stack state)
      (push node (third (car (composer-state-stack state))))
      (if (composer-state-current state)
          (%composer-error "multiple root nodes")
          (setf (composer-state-current state) node))))

(defun %composer-start-collection (state event sequence-p max-depth max-nodes)
  (let ((node (if sequence-p
                 (make-sequence-node :tag (sequence-start-event-tag event)
                                     :anchor (sequence-start-event-anchor event)
                                     :style (sequence-start-event-style event)
                                     :start-mark (event-start-mark event)
                                     :end-mark (event-end-mark event))
                 (make-mapping-node :tag (mapping-start-event-tag event)
                                    :anchor (mapping-start-event-anchor event)
                                    :style (mapping-start-event-style event)
                                    :start-mark (event-start-mark event)
                                    :end-mark (event-end-mark event)))))
    (incf (composer-state-nodes state))
    (%limit! (composer-state-nodes state) max-nodes "nodes")
    (when (node-anchor node)
      (setf (gethash (node-anchor node) (composer-state-anchors state)) node))
    (incf (composer-state-depth state))
    (%limit! (composer-state-depth state) max-depth "depth")
    (push (list (if sequence-p :sequence :mapping) node nil)
          (composer-state-stack state))))

(defun %composer-finish-collection (state event)
  (let ((frame (pop (composer-state-stack state))))
    (unless frame (%composer-error "collection end without start"))
    (destructuring-bind (kind node children) frame
      (unless (eq kind (if (sequence-end-event-p event) :sequence :mapping))
        (%composer-error "collection end does not match start" (event-start-mark event)))
      (setf children (nreverse children))
      (if (eq kind :sequence)
          (setf (sequence-node-items node) children)
          (progn
            (unless (evenp (length children)) (%composer-error "odd mapping entries"))
            (setf (mapping-node-pairs node)
                  (loop for (key value) on children by #'cddr
                        collect (cons key value)))))
      (decf (composer-state-depth state))
      (%composer-append-node state node))))

(defun %composer-finish-document (state)
  (unless (composer-state-document-open-p state)
    (%composer-error "document end outside document"))
  (when (composer-state-stack state)
    (%composer-error "document ended inside collection"))
  (let ((document (composer-state-current state)))
    (unless document (%composer-error "document has no root node"))
    (if (composer-state-document-handler state)
        (funcall (composer-state-document-handler state) document)
        (push document (composer-state-documents state)))
    (setf (composer-state-document-open-p state) nil
          (composer-state-current state) nil)
    (when (composer-state-first-only state)
      (throw 'first-document document))))

(defun %composer-scalar (state event max-scalar-length max-nodes)
  (%limit! (length (scalar-event-value event)) max-scalar-length "scalar")
  (let ((node (make-scalar-node :tag (scalar-event-tag event)
                                :anchor (scalar-event-anchor event)
                                :style (scalar-event-style event)
                                :start-mark (event-start-mark event)
                                :end-mark (event-end-mark event)
                                :value (scalar-event-value event))))
    (incf (composer-state-nodes state))
    (%limit! (composer-state-nodes state) max-nodes "nodes")
    (when (node-anchor node)
      (setf (gethash (node-anchor node) (composer-state-anchors state)) node))
    (%composer-append-node state node)))

(defun %composer-alias (state event max-alias-expansions max-nodes)
  (incf (composer-state-aliases state))
  (%limit! (composer-state-aliases state) max-alias-expansions "alias expansions")
  (multiple-value-bind (node presentp)
      (gethash (alias-event-anchor event) (composer-state-anchors state))
    (unless presentp (%composer-error "unknown alias" (event-start-mark event)))
    (incf (composer-state-nodes state))
    (%limit! (composer-state-nodes state) max-nodes "nodes")
    (%composer-append-node state node)))

(defun %composer-event (state event max-depth max-scalar-length max-nodes max-alias-expansions)
  (cond
    ((stream-start-event-p event)
     (when (or (composer-state-stream-open-p state)
               (composer-state-stream-ended-p state))
       (%composer-error "duplicate stream start"))
     (setf (composer-state-stream-open-p state) t))
    ((stream-end-event-p event)
     (unless (and (composer-state-stream-open-p state)
                  (not (composer-state-document-open-p state)))
       (%composer-error "stream end before document end"))
     (setf (composer-state-stream-ended-p state) t))
    ((document-start-event-p event) (%composer-start-document state))
    ((document-end-event-p event) (%composer-finish-document state))
    ((and (not (composer-state-stream-open-p state))
          (not (composer-state-stream-ended-p state)))
     (%composer-error "event before stream start"))
    ((composer-state-stream-ended-p state) (%composer-error "event after stream end"))
    ((not (composer-state-document-open-p state))
     (%composer-error "event outside document"))
    ((alias-event-p event) (%composer-alias state event max-alias-expansions max-nodes))
    ((scalar-event-p event) (%composer-scalar state event max-scalar-length max-nodes))
    ((sequence-start-event-p event)
     (%composer-start-collection state event t max-depth max-nodes))
    ((mapping-start-event-p event)
     (%composer-start-collection state event nil max-depth max-nodes))
    ((or (sequence-end-event-p event) (mapping-end-event-p event))
     (%composer-finish-collection state event))
    (t (%composer-error "unknown event"))))

(defun compose-all-events (events &key (max-depth +default-max-depth+)
                                     (max-scalar-length +default-max-scalar-length+)
                                     (max-nodes +default-max-nodes+)
                                     (max-alias-expansions +default-max-alias-expansions+)
                                     first-only document-handler)
  "Compose EVENTS from a validated stream/document event source."
  (let ((state (%make-composer-state :first-only first-only
                                     :document-handler document-handler))
        (first-document nil))
    (setf first-document
          (catch 'first-document
            (funcall events
                     (lambda (event)
                       (%composer-event state event max-depth max-scalar-length
                                        max-nodes max-alias-expansions)))))
    (if (and first-only first-document)
        (list first-document)
        (progn
          (unless (or (composer-state-stream-ended-p state)
                      (and (null (composer-state-documents state))
                           (not (composer-state-stream-open-p state))
                           (zerop (composer-state-nodes state))))
            (%composer-error "stream end is required"))
          (when (or (composer-state-document-open-p state)
                    (composer-state-stack state))
            (%composer-error "unclosed document or collection"))
          (if first-only
              (composer-state-documents state)
              (nreverse (composer-state-documents state)))))))

(defun compose-events (events &rest options)
  "Compose the first graph from an event source."
  (car (apply #'compose-all-events
              (cons events (append options '(:first-only t))))))
