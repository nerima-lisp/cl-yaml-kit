(in-package #:yaml-kit)

(defun %format-edit-semantic-sequence-child (container component)
  (if (%format-edit-sequence-index-p component (length container))
      (values (elt container component) t)
      (values nil nil)))

(defun %format-edit-semantic-child (container component)
  (%format-edit-container-case (container) container
    (:hash-table (gethash component container))
    (:yaml-mapping
     (let ((entry (assoc component (yaml-mapping-entries container)
                         :test #'equal)))
       (if entry (values (cdr entry) t) (values nil nil))))
    ((:vector :list)
     (%format-edit-semantic-sequence-child container component))
    (otherwise (values nil nil))))

(defun %format-edit-semantic-copy (value)
  (%format-edit-container-case (value) value
    (:hash-table
     (let ((copy (make-hash-table :test (hash-table-test value))))
       (maphash (lambda (key item)
                  (setf (gethash key copy)
                        (%format-edit-semantic-copy item)))
                value)
       copy))
    (:yaml-mapping
     (make-yaml-mapping
      (mapcar (lambda (entry)
                (cons (car entry) (%format-edit-semantic-copy (cdr entry))))
              (yaml-mapping-entries value))))
    (:vector
     (if (stringp value)
         value
         (map 'vector #'%format-edit-semantic-copy value)))
    (:list
     (if (consp value)
         (mapcar #'%format-edit-semantic-copy value)
         value))
    (otherwise value)))

(defun %format-edit-semantic-sequence-replace-child
    (container component child path)
  (unless (%format-edit-sequence-index-p component (length container))
    (%format-edit-structure-error path "semantic edit index is out of range"))
  (let ((copy (copy-seq container)))
    (setf (elt copy component) child)
    copy))

(defun %format-edit-semantic-replace-child (container component child path)
  (%format-edit-container-case (container) container
    (:hash-table
     (let ((copy (%format-edit-semantic-copy container)))
       (setf (gethash component copy) child)
       copy))
    (:yaml-mapping
     (make-yaml-mapping
      (mapcar (lambda (entry)
                (if (equal (car entry) component)
                    (cons (car entry) child)
                    entry))
              (yaml-mapping-entries container))))
    ((:vector :list)
     (%format-edit-semantic-sequence-replace-child
      container component child path))
    (otherwise
     (%format-edit-structure-error path
                                   "semantic edit parent is not a collection"))))

(defun %format-edit-semantic-change-child
    (value component replacement operation error-path)
  (case operation
    (:set
     (unless (nth-value 1 (%format-edit-semantic-child value component))
       (%format-edit-structure-error
        error-path "edited path is absent from the original value"))
     (%format-edit-semantic-replace-child
      value component replacement error-path))
    (:add
     (%format-edit-semantic-add-child value component replacement error-path))
    (:delete
     (%format-edit-semantic-delete-child value component error-path))))

(defun %format-edit-semantic-change-at
    (value path replacement operation error-path)
  (if (null (cdr path))
      (%format-edit-semantic-change-child
       value (car path) replacement operation error-path)
      (multiple-value-bind (child presentp)
          (%format-edit-semantic-child value (car path))
        (unless presentp
          (%format-edit-structure-error
           error-path
           (case operation
             (:set "edited path is absent from the original value")
             (:add "an intermediate path component is absent")
             (:delete "deleted path is absent"))))
        (%format-edit-semantic-replace-child
         value (car path)
         (%format-edit-semantic-change-at
          child (cdr path) replacement operation error-path)
         error-path))))

(defun %format-edit-semantic-sequence-add-child
    (container component child path)
  (unless (%format-edit-sequence-index-p component (length container) t)
    (%format-edit-structure-error
     path "semantic sequence edit is not append-only"))
  (if (vectorp container)
      (concatenate 'vector container (vector child))
      (append container (list child))))

(defun %format-edit-semantic-add-child (container component child path)
  (%format-edit-container-case (container) container
    (:hash-table
     (when (nth-value 1 (gethash component container))
       (%format-edit-structure-error path "added path already exists"))
     (let ((copy (%format-edit-semantic-copy container)))
       (setf (gethash component copy) child)
       copy))
    (:yaml-mapping
     (when (assoc component (yaml-mapping-entries container) :test #'equal)
       (%format-edit-structure-error path "added path already exists"))
     (make-yaml-mapping
      (append (yaml-mapping-entries container)
              (list (cons component child)))))
    ((:vector :list)
     (%format-edit-semantic-sequence-add-child container component child path))
    (otherwise
     (%format-edit-structure-error path
                                   "semantic edit parent is not a collection"))))

(defun %format-edit-semantic-sequence-delete-child
    (container component path)
  (unless (%format-edit-sequence-index-p component (length container))
    (%format-edit-structure-error path "semantic edit index is out of range"))
  (if (vectorp container)
      (concatenate 'vector (subseq container 0 component)
                   (subseq container (1+ component)))
      (append (subseq container 0 component)
              (nthcdr (1+ component) container))))

(defun %format-edit-semantic-delete-child (container component path)
  (%format-edit-container-case (container) container
    (:hash-table
     (unless (nth-value 1 (gethash component container))
       (%format-edit-structure-error path "deleted path is absent"))
     (let ((copy (%format-edit-semantic-copy container)))
       (remhash component copy)
       copy))
    (:yaml-mapping
     (unless (assoc component (yaml-mapping-entries container) :test #'equal)
       (%format-edit-structure-error path "deleted path is absent"))
     (make-yaml-mapping
      (remove component (yaml-mapping-entries container)
              :key #'car :test #'equal)))
    ((:vector :list)
     (%format-edit-semantic-sequence-delete-child container component path))
    (otherwise
     (%format-edit-structure-error path
                                   "semantic edit parent is not a collection"))))

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
    ((or (stringp left) (stringp right))
     (and (stringp left) (stringp right) (string= left right)))
    ((and (vectorp left) (vectorp right))
     (and (= (length left) (length right))
          (loop for index below (length left)
                always (%format-edit-semantic-equal-p
                        (aref left index) (aref right index)))))
    ((and (consp left) (consp right))
     (and (= (length left) (length right))
          (every #'%format-edit-semantic-equal-p left right)))
    (t (equal left right))))

(defun %format-edit-semantic-expected (original path value operation document)
  (let ((expected (mapcar #'%format-edit-semantic-copy original)))
    (unless (< document (length expected))
      (%format-edit-structure-error path "semantic edit document is absent"))
    (setf (nth document expected)
          (case operation
            (:set
             (%format-edit-semantic-change-at
              (nth document expected) path value :set path))
            (:delete
             (%format-edit-semantic-change-at
              (nth document expected) path nil :delete path))
            (:add
             (%format-edit-semantic-change-at
              (nth document expected) path value :add path))))
    expected))

(defun %format-edit-validate-semantics
    (original edited path value operation document)
  (let ((expected (%format-edit-semantic-expected
                   original path value operation document)))
    (unless (%format-edit-semantic-equal-p expected edited)
      (%format-edit-structure-error
       path "edited YAML changed a value outside the requested edit"))))
