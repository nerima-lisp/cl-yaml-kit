(in-package #:yaml-kit)

(defun %format-edit-proper-list-p (object)
  (loop for tail = object then (cdr tail)
        while (consp tail)
        finally (return (null tail))))

(defun edit-source (source path value &key (document 0) (operation :set))
  (unless (%format-edit-proper-list-p path)
    (%format-edit-path-error path document operation
                        "path must be a proper list"))
  (unless (and (integerp document) (<= 0 document))
    (%format-edit-path-error path document operation
                        "document must be a non-negative integer"))
  (unless (member operation '(:set :delete))
    (%format-edit-path-error path document operation
                        "operation must be :set or :delete"))
  (when (null path)
    (%format-edit-path-error path document operation
                        "path must not be empty"))
  (multiple-value-bind (text encoder)
      (%format-edit-source-text source path document operation)
    (let ((original-values nil))
      (labels ((finish (edited semantic-operation)
               (multiple-value-bind (validated edited-values)
                   (%format-edit-validate-text edited path)
                 (%format-edit-validate-semantics
                  original-values edited-values path value
                  semantic-operation document)
                 (%format-edit-encode encoder validated))))
      (let* ((events (parse-events text))
             (roots (%format-edit-node-from-events events))
             (root (nth document roots)))
        (unless root
          (%format-edit-path-error path document operation
                              "document does not exist"))
        (multiple-value-bind (node parent component)
            (%format-edit-find root path document operation)
          (setf original-values (parse-all text))
          (cond
            (node
             (%format-edit-check-safe-node node path)
             (if (eq operation :delete)
                 (progn
                   (when (%format-edit-only-child-p parent)
                     (%format-edit-structure-error path
                                                   "deleting the only collection child is not supported"))
                   (when (%format-edit-inline-sequence-node-p parent text)
                     (%format-edit-structure-error
                      path "deleting entries from inline sequence nodes is not supported"))
                   (let ((entry (if (eq (format-edit-node-kind parent) :mapping)
                                    (find-if (lambda (pair)
                                               (eq (format-edit-pair-value pair) node))
                                             (format-edit-node-pairs parent))
                                    node)))
                     (multiple-value-bind (start end)
                         (%format-edit-delete-entry text parent entry path)
                       (finish (%format-edit-apply-replacement text start end "")
                               :delete))))
                 (progn
                   (%format-edit-check-set-node text node path)
                   (let ((replacement
                           (%format-edit-fragment
                            value
                            (and parent
                                 (eq (format-edit-node-style parent) :flow)))))
                     (finish
                      (%format-edit-apply-replacement
                       text (format-edit-node-start node)
                       (format-edit-node-end node) replacement)
                      :set)))))
            ((eq operation :delete)
             (%format-edit-path-error path document operation
                                 "path does not exist"))
            (t
             (%format-edit-check-safe-node parent path)
             (multiple-value-bind (position insertion)
                 (%format-edit-set-missing text parent component value path document)
               (let ((edited (%format-edit-apply-replacement text position position insertion)))
                 (finish (%format-edit-preserve-final-newline text edited)
                         :add)))))))))))
