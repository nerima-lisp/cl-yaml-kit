(in-package #:yaml-kit)

(defun %format-edit-semantic-child (container component)
  (cond
    ((hash-table-p container)
     (gethash component container))
    ((yaml-mapping-p container)
     (let ((entry (assoc component (yaml-mapping-entries container) :test #'equal)))
       (if entry (values (cdr entry) t) (values nil nil))))
    ((vectorp container)
     (if (and (integerp component) (<= 0 component)
              (< component (length container)))
         (values (aref container component) t)
         (values nil nil)))
    ((listp container)
     (if (and (integerp component) (<= 0 component)
              (< component (length container)))
         (values (nth component container) t)
         (values nil nil)))
    (t (values nil nil))))

(defun %format-edit-semantic-copy (value)
  (cond
    ((hash-table-p value)
     (let ((copy (make-hash-table :test (hash-table-test value))))
       (maphash (lambda (key item)
                  (setf (gethash key copy)
                        (%format-edit-semantic-copy item)))
                value)
       copy))
    ((yaml-mapping-p value)
     (make-yaml-mapping
      (mapcar (lambda (entry)
                (cons (car entry) (%format-edit-semantic-copy (cdr entry))))
              (yaml-mapping-entries value))))
    ((stringp value) value)
    ((vectorp value)
     (map 'vector #'%format-edit-semantic-copy value))
    ((consp value)
     (mapcar #'%format-edit-semantic-copy value))
    (t value)))

(defun %format-edit-semantic-replace-child (container component child path)
  (cond
    ((hash-table-p container)
     (let ((copy (%format-edit-semantic-copy container)))
       (setf (gethash component copy) child)
       copy))
    ((yaml-mapping-p container)
     (let ((entries
             (mapcar (lambda (entry)
                       (if (equal (car entry) component)
                           (cons (car entry) child)
                           entry))
                     (yaml-mapping-entries container))))
       (make-yaml-mapping entries)))
    ((vectorp container)
     (unless (and (integerp component) (<= 0 component)
                  (< component (length container)))
       (%format-edit-structure-error path "semantic edit index is out of range"))
     (let ((copy (copy-seq container)))
       (setf (aref copy component) child)
       copy))
    ((listp container)
     (unless (and (integerp component) (<= 0 component)
                  (< component (length container)))
       (%format-edit-structure-error path "semantic edit index is out of range"))
     (let ((copy (copy-list container)))
       (setf (nth component copy) child)
       copy))
    (t (%format-edit-structure-error path "semantic edit parent is not a collection"))))

(defun %format-edit-semantic-set-at (value path replacement error-path)
  (if (null path)
      replacement
      (multiple-value-bind (child presentp)
          (%format-edit-semantic-child value (car path))
        (unless presentp
          (%format-edit-structure-error error-path
                                        "edited path is absent from the original value"))
        (%format-edit-semantic-replace-child
         value (car path)
         (%format-edit-semantic-set-at child (cdr path) replacement error-path)
         error-path))))

(defun %format-edit-semantic-add-child (container component child path)
  (cond
    ((hash-table-p container)
     (when (nth-value 1 (gethash component container))
       (%format-edit-structure-error path "added path already exists"))
     (let ((copy (%format-edit-semantic-copy container)))
       (setf (gethash component copy) child)
       copy))
    ((yaml-mapping-p container)
     (when (assoc component (yaml-mapping-entries container) :test #'equal)
       (%format-edit-structure-error path "added path already exists"))
     (make-yaml-mapping
      (append (yaml-mapping-entries container)
              (list (cons component child)))))
    ((vectorp container)
     (unless (and (integerp component) (<= 0 component)
                  (= component (length container)))
       (%format-edit-structure-error path "semantic sequence edit is not append-only"))
     (concatenate 'vector container (vector child)))
    ((listp container)
     (unless (and (integerp component) (<= 0 component)
                  (= component (length container)))
       (%format-edit-structure-error path "semantic sequence edit is not append-only"))
     (append container (list child)))
    (t (%format-edit-structure-error path "semantic edit parent is not a collection"))))

(defun %format-edit-semantic-add-at (value path replacement error-path)
  (when (null path)
    (%format-edit-structure-error error-path "cannot add at an empty path"))
  (if (null (cdr path))
      (%format-edit-semantic-add-child value (car path) replacement error-path)
      (multiple-value-bind (child presentp)
          (%format-edit-semantic-child value (car path))
        (unless presentp
          (%format-edit-structure-error error-path
                                        "an intermediate path component is absent"))
        (%format-edit-semantic-replace-child
         value (car path)
         (%format-edit-semantic-add-at child (cdr path) replacement error-path)
         error-path))))

(defun %format-edit-semantic-delete-child (container component path)
  (cond
    ((hash-table-p container)
     (unless (nth-value 1 (gethash component container))
       (%format-edit-structure-error path "deleted path is absent"))
     (let ((copy (%format-edit-semantic-copy container)))
       (remhash component copy)
       copy))
    ((yaml-mapping-p container)
     (unless (assoc component (yaml-mapping-entries container) :test #'equal)
       (%format-edit-structure-error path "deleted path is absent"))
     (make-yaml-mapping
      (remove component (yaml-mapping-entries container)
              :key #'car :test #'equal)))
    ((vectorp container)
     (unless (and (integerp component) (<= 0 component)
                  (< component (length container)))
       (%format-edit-structure-error path "semantic edit index is out of range"))
     (concatenate 'vector (subseq container 0 component)
                  (subseq container (1+ component))))
    ((listp container)
     (unless (and (integerp component) (<= 0 component)
                  (< component (length container)))
       (%format-edit-structure-error path "semantic edit index is out of range"))
     (append (subseq container 0 component) (nthcdr (1+ component) container)))
    (t (%format-edit-structure-error path "semantic edit parent is not a collection"))))

(defun %format-edit-semantic-delete-at (value path error-path)
  (when (null path)
    (%format-edit-structure-error error-path "cannot delete at an empty path"))
  (if (null (cdr path))
      (%format-edit-semantic-delete-child value (car path) error-path)
      (multiple-value-bind (child presentp)
          (%format-edit-semantic-child value (car path))
        (unless presentp
          (%format-edit-structure-error error-path "deleted path is absent"))
        (%format-edit-semantic-replace-child
         value (car path)
         (%format-edit-semantic-delete-at child (cdr path) error-path)
         error-path))))

(defun %format-edit-semantic-equal-p (left right)
  (cond
    ((and (hash-table-p left) (hash-table-p right))
     (and (= (hash-table-count left) (hash-table-count right))
          (block mismatch
            (maphash (lambda (key value)
                       (multiple-value-bind (other presentp) (gethash key right)
                         (unless (and presentp
                                      (%format-edit-semantic-equal-p value other))
                           (return-from mismatch nil))))
                     left)
            t)))
    ((and (yaml-mapping-p left) (yaml-mapping-p right))
     (let ((left-entries (yaml-mapping-entries left))
           (right-entries (yaml-mapping-entries right)))
       (and (= (length left-entries) (length right-entries))
            (every (lambda (entry)
                    (let ((other (assoc (car entry) right-entries :test #'equal)))
                      (and other
                           (%format-edit-semantic-equal-p
                            (cdr entry) (cdr other)))))
                   left-entries))))
    ((and (vectorp left) (vectorp right))
     (and (= (length left) (length right))
          (loop for index below (length left)
                always (%format-edit-semantic-equal-p
                        (aref left index) (aref right index)))))
    ((and (consp left) (consp right))
     (and (= (length left) (length right))
          (every #'%format-edit-semantic-equal-p left right)))
    (t (equalp left right))))

(defun %format-edit-semantic-expected (original path value operation document)
  (let ((expected (mapcar #'%format-edit-semantic-copy original)))
    (unless (and (integerp document) (<= 0 document) (< document (length expected)))
      (%format-edit-structure-error path "semantic edit document is absent"))
    (setf (nth document expected)
          (case operation
            (:set (%format-edit-semantic-set-at
                   (nth document expected) path value path))
            (:delete (%format-edit-semantic-delete-at
                      (nth document expected) path path))
            (:add
             (%format-edit-semantic-add-at
              (nth document expected) path value path))
            (otherwise
             (%format-edit-structure-error path "unknown semantic edit operation"))))
    expected))

(defun %format-edit-validate-semantics
    (original edited path value operation document)
  (let ((expected (%format-edit-semantic-expected
                   original path value operation document)))
    (unless (%format-edit-semantic-equal-p expected edited)
      (%format-edit-structure-error
       path "edited YAML changed a value outside the requested edit"))))
