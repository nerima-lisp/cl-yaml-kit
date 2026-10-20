(in-package #:yaml-kit)

(defun %emit-dispatch (event state lookahead)
  (typecase event
    (stream-start-event (%emit-stream-start-event event state lookahead))
    (stream-end-event (%emit-stream-end-event event state lookahead))
    (document-start-event (%emit-document-start-event event state lookahead))
    (document-end-event (%emit-document-end-event event state lookahead))
    (scalar-event (%emit-scalar-event event state lookahead))
    (alias-event (%emit-alias-event event state lookahead))
    (sequence-start-event
     (%emit-sequence-start-frame state event (car lookahead)))
    (sequence-end-event (%emit-sequence-end-frame state))
    (mapping-start-event
     (%emit-mapping-start-frame state event (car lookahead)))
    (mapping-end-event (%emit-mapping-end-frame state))
    (t (signal-yaml-emit-error :context "unknown event"
                               :cause "unknown event"))))
(defun emit-event-stream (events stream &key (indent 2)
                                      (explicit-document-start nil)
                                      (suppress-empty-document-marker nil))
  (let ((state (make-emitter-frame-state (make-emitter-context stream indent) indent
                                          explicit-document-start suppress-empty-document-marker))
        (queue nil)
        (queue-tail nil)
        (queue-count 0))
    (labels ((dequeue ()
               (prog1 (car queue)
                 (setf queue (cdr queue)
                       queue-count (1- queue-count))
                 (when (null queue)
                   (setf queue-tail nil))))
             (consume (event)
               (when event
                 (let ((cell (list event)))
                   (if queue-tail
                       (setf (cdr queue-tail) cell)
                       (setf queue cell))
                   (setf queue-tail cell)
                   (incf queue-count)
                   (when (> queue-count 2)
                     (%emit-dispatch (dequeue) state queue))))))
      (funcall events #'consume)
      (loop while queue
            do (let ((current (dequeue)))
                 (%emit-dispatch current state queue))))
    stream))
(defun %emitter-emit-event-stream/cps
    (events stream k &key (indent 2)
                          (explicit-document-start nil)
                          (suppress-empty-document-marker nil))
  (unless (functionp k)
    (error 'type-error :datum k :expected-type 'function))
  (emit-event-stream events stream
                     :indent indent
                     :explicit-document-start explicit-document-start
                     :suppress-empty-document-marker
                     suppress-empty-document-marker)
  (funcall k nil))
