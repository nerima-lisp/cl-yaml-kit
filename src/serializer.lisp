;;;; src/serializer.lisp
(in-package #:yaml-kit)

(defun %walk-node-children (node function)
  (cond ((sequence-node-p node)
         (dolist (child (sequence-node-items node)) (funcall function child)))
        ((mapping-node-p node)
         (dolist (pair (mapping-node-pairs node))
           (funcall function (car pair)) (funcall function (cdr pair))))))

(defun %assign-anchors (root)
  (let ((counts (make-hash-table :test #'eq))
        (seen (make-hash-table :test #'eq)))
    (labels ((count-node (node)
               (unless (or (scalar-node-p node)
                           (sequence-node-p node)
                           (mapping-node-p node))
                 (signal-yaml-emit-error :diagnostic (cl-parser-kit:make-diagnostic :message (princ-to-string (type-of node)) :span (node-start-span node)) :cause "unsupported representation node"))
               (incf (gethash node counts 0))
               (unless (gethash node seen)
                 (setf (gethash node seen) t)
                  (%walk-node-children node #'count-node))))
      (count-node root)
      (clrhash seen)
      (let ((counter 0))
        (labels ((mark-node (node)
                   (unless (gethash node seen)
                     (setf (gethash node seen) t)
                     (setf (node-anchor node)
                           (when (> (gethash node counts 0) 1)
                             (concatenate 'simple-string
                                          "id"
                                          (write-to-string (incf counter)))))
                      (%walk-node-children node #'mark-node))))
          (mark-node root)))))
  root)

(defun %serializer-walk-node (node handler span active emitted)
  (labels ((send (event) (funcall handler event))
           (alias ()
             (send (make-alias-event :start-span span :end-span span
                                     :anchor (node-anchor node))))
           (walk (child)
             (%serializer-walk-node child handler span active emitted)))
    (when (or (gethash node emitted) (gethash node active))
      (return-from %serializer-walk-node (alias)))
    (when (or (sequence-node-p node) (mapping-node-p node))
      (setf (gethash node active) t))
    (let ((anchor (node-anchor node))
          (tag (node-tag node)))
      (cond
        ((scalar-node-p node)
         (send (make-scalar-event :start-span span :end-span span
                                  :anchor anchor :tag tag
                                  :value (scalar-node-value node)
                                  :style (or (node-style node) :plain))))
        ((sequence-node-p node)
         (send (make-sequence-start-event :start-span span :end-span span
                                          :anchor anchor :tag tag
                                          :style (or (node-style node) :block)))
         (%walk-node-children node #'walk)
         (send (make-sequence-end-event :start-span span :end-span span)))
        ((mapping-node-p node)
         (send (make-mapping-start-event :start-span span :end-span span
                                         :anchor anchor :tag tag
                                         :style (or (node-style node) :block)))
         (%walk-node-children node #'walk)
         (send (make-mapping-end-event :start-span span :end-span span)))))
    (remhash node active)
    (setf (gethash node emitted) t)))
(defun serialize (root handler &key (explicit-document-start nil))
  "Deliver serialized events to HANDLER in CPS order."
  (let* ((active (make-hash-table :test #'eq))
         (emitted (make-hash-table :test #'eq))
         (span (cl-parser-kit:make-span :source nil :start 0 :end 0 :start-line 1 :start-column 1 :end-line 1 :end-column 1)))
    (%assign-anchors root)
    (funcall handler (make-stream-start-event :start-span span :end-span span))
    (funcall handler (make-document-start-event :start-span span :end-span span
                                                :explicit-p explicit-document-start))
    (%serializer-walk-node root handler span active emitted)
    (funcall handler (make-document-end-event :start-span span :end-span span))
    (funcall handler (make-stream-end-event :start-span span :end-span span))))
(defun %serializer-serialize/cps
    (root handler k &key (explicit-document-start nil))
  (unless (functionp k)
    (error 'type-error :datum k :expected-type 'function))
  (serialize root handler
             :explicit-document-start explicit-document-start)
  (funcall k nil))
