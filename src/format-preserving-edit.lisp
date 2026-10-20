(in-package #:yaml-kit)

(defun %format-edit-proper-list-p (object)
  (loop for tail = object then (cdr tail)
        while (consp tail)
        finally (return (null tail))))

(defun %format-edit-validate-request (path document operation)
  (unless (%format-edit-proper-list-p path)
    (%format-edit-path-error path document operation "path must be a proper list"))
  (unless (and (integerp document) (<= 0 document))
    (%format-edit-path-error
     path document operation "document must be a non-negative integer"))
  (unless (member operation '(:set :delete))
    (%format-edit-path-error path document operation
                             "operation must be :set or :delete"))
  (when (null path)
    (%format-edit-path-error path document operation "path must not be empty")))

(defun %format-edit-finish (edited path value original-values semantic-operation
                            document encoder)
  (multiple-value-bind (validated edited-values)
      (%format-edit-validate-text edited path)
    (%format-edit-validate-semantics
     original-values edited-values path value semantic-operation document)
    (%format-edit-encode encoder validated)))

(defun %format-edit-delete-node (text node parent path value original-values
                                document encoder)
  (when (%format-edit-only-child-p parent)
    (%format-edit-structure-error
     path "deleting the only collection child is not supported"))
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
      (%format-edit-finish
       (%format-edit-apply-replacement text start end "")
       path value original-values :delete document encoder))))

(defun %format-edit-set-node (text node parent path value original-values
                             document encoder)
  (%format-edit-check-set-node text node path)
  (let ((replacement
          (%format-edit-fragment
            value path
            (and parent
                 (eq (format-edit-node-style parent) :flow)))))
    (%format-edit-finish
     (%format-edit-apply-replacement
      text (format-edit-node-start node) (format-edit-node-end node) replacement)
     path value original-values :set document encoder)))

(defun %format-edit-edit-existing-node
    (text node parent path value operation original-values document encoder)
  (%format-edit-check-safe-node node path)
  (if (eq operation :delete)
      (%format-edit-delete-node
       text node parent path value original-values document encoder)
      (%format-edit-set-node
       text node parent path value original-values document encoder)))

(defun %format-edit-edit-missing-node
    (text parent component path value operation original-values document encoder)
  (when (eq operation :delete)
    (%format-edit-path-error path document operation "path does not exist"))
  (%format-edit-check-safe-node parent path)
  (multiple-value-bind (position insertion)
      (%format-edit-set-missing text parent component value path document)
    (%format-edit-finish
     (%format-edit-preserve-final-newline
      text (%format-edit-apply-replacement text position position insertion))
     path value original-values :add document encoder)))

(defun %format-edit-source (source path value &key (document 0) (operation :set))
  (%format-edit-validate-request path document operation)
  (multiple-value-bind (text encoder)
      (%format-edit-source-text source path document operation)
    (let* ((events (parse-events text))
           (roots (%format-edit-node-from-events events))
           (root (nth document roots)))
      (unless root
        (%format-edit-path-error path document operation "document does not exist"))
      (multiple-value-bind (node parent component)
          (%format-edit-find root path document operation)
        (let ((original-values (parse-all text)))
          (if node
              (%format-edit-edit-existing-node
               text node parent path value operation original-values document encoder)
              (%format-edit-edit-missing-node
               text parent component path value operation original-values
               document encoder)))))))
(defun %format-edit-source/cps
    (source path value &key (document 0) (operation :set) (k nil))
  (check-type k function)
  (funcall k (%format-edit-source source path value
                                  :document document
                                  :operation operation)))
(progn
  (defun edit-source/k
    (source path value &key (document 0) (operation :set) (k nil))
  "Return the primary value produced by K; K runs once after a successful edit."
  (check-type k function)
  (values (%format-edit-source/cps
           source path value :document document :operation operation :k k)))

  (defun edit-source (source path value &key (document 0) (operation :set))
    "Return the source with the requested scalar edit and its surrounding format preserved."
    (edit-source/k source path value
                   :document document
                   :operation operation
                   :k #'identity)))
