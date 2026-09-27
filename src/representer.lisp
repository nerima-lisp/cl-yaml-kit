;;;; src/representer.lisp
(in-package #:yaml-kit)

(defvar *representer-cache* nil)
(declaim (ftype function %represent-dispatch))

(defun %represent-scalar (value)
  (make-scalar-node :tag "tag:yaml.org,2002:str"
                    :value value
                    :style (when (zerop (length value)) :single-quoted)))

(defun %represent-number (value)
  (make-scalar-node :tag (if (integerp value)
                             "tag:yaml.org,2002:int"
                             "tag:yaml.org,2002:float")
                    :value
                    (cond ((and (floatp value) (sb-ext:float-infinity-p value))
                           (if (minusp value) "-.inf" ".inf"))
                          ((and (floatp value) (sb-ext:float-nan-p value)) ".nan")
                          (t (let ((*read-default-float-format* 'double-float))
                               (string-downcase (princ-to-string value)))))))

(defmacro define-representer-builder (name constructor accessor &body forms)
  `(defun ,name (value)
     (or (gethash value *representer-cache*)
         (let ((node (,constructor)))
           (setf (gethash value *representer-cache*) node
                 (,accessor node) (progn ,@forms))
           node))))

(define-representer-builder %represent-list
  make-sequence-node
  sequence-node-items
  (mapcar #'%represent-dispatch value))

(define-representer-builder %represent-vector
  make-sequence-node
  sequence-node-items
  (loop for item across value collect (%represent-dispatch item)))

(define-representer-builder %represent-mapping
  make-mapping-node
  mapping-node-pairs
  (loop for (key . item) in (yaml-mapping-entries value)
        collect (cons (%represent-dispatch key) (%represent-dispatch item))))

(defun %represent-hash-table (value)
  (or (gethash value *representer-cache*)
      (let ((node (make-mapping-node :pairs nil)) (pairs nil))
        (setf (gethash value *representer-cache*) node)
        (maphash (lambda (key item)
                   (push (cons (%represent-dispatch key)
                               (%represent-dispatch item)) pairs)) value)
        (setf (mapping-node-pairs node) (nreverse pairs))
        node)))

(defun %represent-unsupported (value)
  (error 'yaml-emit-error
         :context "unsupported value type"
         :message (princ-to-string (type-of value))))

(defmacro define-representer-dispatch (name clauses)
  "Define NAME from a declarative type/function table."
  `(defun ,name (value)
     (declare (optimize (speed 3) (safety 1)))
     (handler-case
         (etypecase value
           ,@(loop for (type function) in clauses
                   collect `(,type (,function value))))
       (type-error () (%represent-unsupported value)))))

(define-representer-dispatch %represent-dispatch
  ((null (lambda (value) (declare (ignore value))
           (make-sequence-node :items nil)))
   ((eql t) (lambda (value) (declare (ignore value))
              (make-scalar-node :tag "tag:yaml.org,2002:bool" :value "true")))
   (yaml-sentinel (lambda (value)
                    (make-scalar-node
                     :tag (if (yaml-null-p value)
                              "tag:yaml.org,2002:null"
                              "tag:yaml.org,2002:bool")
                     :value (if (yaml-null-p value) "null" "false"))))
   (yaml-mapping %represent-mapping)
   (hash-table %represent-hash-table)
   (string %represent-scalar)
   (character (lambda (value) (%represent-scalar (string value))))
   (integer %represent-number)
   (float %represent-number)
   (vector %represent-vector)
   (cons %represent-list)))

(defun represent (value)
  (let ((*representer-cache* (or *representer-cache*
                                (make-hash-table :test #'eq))))
    (%represent-dispatch value)))
