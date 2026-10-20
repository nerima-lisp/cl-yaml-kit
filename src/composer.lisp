(in-package #:yaml-kit)

(defmacro %call-compose-all-events
    (function events max-depth max-scalar-length max-nodes max-alias-expansions
     first-only document-handler)
  (let ((function* (gensym "FUNCTION"))
        (events* (gensym "EVENTS"))
        (max-depth* (gensym "MAX-DEPTH"))
        (max-scalar-length* (gensym "MAX-SCALAR-LENGTH"))
        (max-nodes* (gensym "MAX-NODES"))
        (max-alias-expansions* (gensym "MAX-ALIAS-EXPANSIONS"))
        (first-only* (gensym "FIRST-ONLY"))
        (document-handler* (gensym "DOCUMENT-HANDLER")))
    `(let ((,function* ,function)
           (,events* ,events)
           (,max-depth* ,max-depth)
           (,max-scalar-length* ,max-scalar-length)
           (,max-nodes* ,max-nodes)
           (,max-alias-expansions* ,max-alias-expansions)
           (,first-only* ,first-only)
           (,document-handler* ,document-handler))
       (funcall ,function* ,events*
                :max-depth ,max-depth*
                :max-scalar-length ,max-scalar-length*
                :max-nodes ,max-nodes*
                :max-alias-expansions ,max-alias-expansions*
                :first-only ,first-only*
                :document-handler ,document-handler*))))

(defstruct (composer-state (:constructor %make-composer-state))
  documents current stack anchors
  (nodes 0) (depth 0) (aliases 0)
  stream-open-p document-open-p stream-ended-p first-only document-handler)

(defun event-list-source (events)
  (lambda (handler)
    (dolist (event events) (funcall handler event))))

(defun %limit! (value limit &optional name span)
  (when (and limit (> value limit))
    (%signal-yaml-resource-limit name limit value :span span :context "YAML")))

(defun %composer-error (cause &optional span)
  (signal-yaml-compose-error
   :diagnostic (cl-parser-kit:make-diagnostic :message cause :span span)
   :cause cause))

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
                  (make-sequence-node :tag (%event-attribute event :tag)
                                      :anchor (%event-attribute event :anchor)
                                      :style (%event-attribute event :style)
                                      :start-span (event-start-span event)
                                      :end-span (event-end-span event))
                  (make-mapping-node :tag (%event-attribute event :tag)
                                     :anchor (%event-attribute event :anchor)
                                     :style (%event-attribute event :style)
                                     :start-span (event-start-span event)
                                     :end-span (event-end-span event)))))
    (incf (composer-state-nodes state))
    (%limit! (composer-state-nodes state) max-nodes "nodes" (event-start-span event))
    (when (node-anchor node)
      (setf (gethash (node-anchor node) (composer-state-anchors state)) node))
    (incf (composer-state-depth state))
    (%limit! (composer-state-depth state) max-depth "depth" (event-start-span event))
    (push (list (if sequence-p :sequence :mapping) node nil)
          (composer-state-stack state))))

(defun %composer-finish-collection (state event collection-kind)
  (let ((frame (pop (composer-state-stack state))))
    (unless frame (%composer-error "collection end without start"))
    (destructuring-bind (kind node children) frame
      (unless (eq kind collection-kind)
        (%composer-error "collection end does not match start" (event-start-span event)))
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

(defun %composer-finish-document (state first-document-tag)
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
      (throw first-document-tag document))))

(defun %composer-scalar (state event max-scalar-length max-nodes)
  (%limit! (length (scalar-event-value event))
           max-scalar-length
           "scalar"
           (event-start-span event))
  (let ((node (make-scalar-node :tag (%event-attribute event :tag)
                                :anchor (%event-attribute event :anchor)
                                :style (%event-attribute event :style)
                                :start-span (event-start-span event)
                                :end-span (event-end-span event)
                                :value (scalar-event-value event))))
    (incf (composer-state-nodes state))
    (%limit! (composer-state-nodes state) max-nodes "nodes" (event-start-span event))
    (when (node-anchor node)
      (setf (gethash (node-anchor node) (composer-state-anchors state)) node))
    (%composer-append-node state node)))

(defun %composer-alias (state event max-alias-expansions max-nodes)
  (incf (composer-state-aliases state))
  (%limit! (composer-state-aliases state)
           max-alias-expansions
           "alias expansions"
           (event-start-span event))
  (let ((anchor (%event-attribute event :anchor)))
    (multiple-value-bind (node presentp)
        (gethash anchor (composer-state-anchors state))
      (unless presentp (%composer-error "unknown alias" (event-start-span event)))
      (incf (composer-state-nodes state))
      (%limit! (composer-state-nodes state) max-nodes "nodes" (event-start-span event))
      (%composer-append-node state node))))

(defun %composer-event (state event max-depth max-scalar-length max-nodes
                        max-alias-expansions first-document-tag)
  (typecase event
    (stream-start-event
     (when (or (composer-state-stream-open-p state)
               (composer-state-stream-ended-p state))
       (%composer-error "duplicate stream start"))
     (setf (composer-state-stream-open-p state) t))
    (stream-end-event
     (unless (and (composer-state-stream-open-p state)
                  (not (composer-state-document-open-p state)))
       (%composer-error "stream end before document end"))
     (setf (composer-state-stream-ended-p state) t))
    (document-start-event
     (%composer-start-document state))
    (document-end-event
     (%composer-finish-document state first-document-tag))
    (t
     (when (and (not (composer-state-stream-open-p state))
                (not (composer-state-stream-ended-p state)))
       (%composer-error "event before stream start"))
     (when (composer-state-stream-ended-p state)
       (%composer-error "event after stream end"))
     (unless (composer-state-document-open-p state)
       (%composer-error "event outside document"))
     (typecase event
       (alias-event
        (%composer-alias state event max-alias-expansions max-nodes))
       (scalar-event
        (%composer-scalar state event max-scalar-length max-nodes))
       (sequence-start-event
        (%composer-start-collection state event t max-depth max-nodes))
       (mapping-start-event
        (%composer-start-collection state event nil max-depth max-nodes))
       (sequence-end-event
        (%composer-finish-collection state event :sequence))
       (mapping-end-event
        (%composer-finish-collection state event :mapping))
       (t (%composer-error "unknown event"))))))

(define-yaml-resource-function %composer-compose-all-events/cps (events k)
    (:group :composer :keywords ((first-only nil) (document-handler nil)))
  (check-type k function)
  (values
   (funcall k
            (%call-compose-all-events #'compose-all-events events
                                      max-depth max-scalar-length
                                      max-nodes max-alias-expansions
                                      first-only document-handler))))

(define-yaml-resource-function compose-all-events (events)
    (:group :composer :keywords ((first-only nil) (document-handler nil)))
  "Compose EVENTS from a validated stream/document event source."
  (let* ((state (%make-composer-state :first-only first-only
                                      :document-handler document-handler))
         (first-document-tag (gensym "FIRST-DOCUMENT"))
         (no-first-document (gensym "NO-FIRST-DOCUMENT"))
         (first-document
           (catch first-document-tag
             (funcall events
                      (lambda (event)
                        (%composer-event state event max-depth max-scalar-length
                                         max-nodes max-alias-expansions
                                         first-document-tag)))
             no-first-document)))
    (if (and first-only
             (not (eq first-document no-first-document)))
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
  (let ((options (loop for (key value) on options by #'cddr
                       unless (eq key :first-only)
                         append (list key value))))
    (car (apply #'compose-all-events
                (append (list events :first-only t) options)))))
(define-yaml-resource-function compose-events/k (event-source k)
    (:group :composer :keywords ())
  (check-type k function)
  (let ((documents
          (%call-compose-all-events #'compose-all-events event-source
                                    max-depth max-scalar-length
                                    max-nodes max-alias-expansions t nil)))
    (values (funcall k (car documents)))))
(define-yaml-resource-function compose-all-events/k (event-source k)
    (:group :composer :keywords ((first-only nil) (document-handler nil)))
  (check-type k function)
  (values
   (funcall k
            (%call-compose-all-events #'compose-all-events event-source
                                      max-depth max-scalar-length
                                      max-nodes max-alias-expansions
                                      first-only document-handler))))
