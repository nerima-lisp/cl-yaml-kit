;;;; src/representer.lisp
(in-package #:yaml-kit)

(defun %represent-scalar (value)
  (make-scalar-node :value value))

(defun %represent-number (value)
  (make-scalar-node :value
                    (cond ((and (floatp value) (sb-ext:float-infinity-p value))
                           (if (minusp value) "-.inf" ".inf"))
                          ((and (floatp value) (sb-ext:float-nan-p value)) ".nan")
                          (t (let ((*read-default-float-format* 'double-float))
                               (string-downcase (princ-to-string value)))))))

(defun %represent-list (value)
  (make-sequence-node :items (mapcar #'represent value)))

(defun %represent-vector (value)
  (make-sequence-node :items (loop for item across value collect (represent item))))

(defun %represent-mapping (value)
  (make-mapping-node
   :pairs (loop for (key . item) in (yaml-mapping-entries value)
                collect (cons (represent key) (represent item)))))

(defun %represent-hash-table (value)
  (let ((pairs nil))
    (maphash (lambda (key item)
              (push (cons (represent key) (represent item)) pairs)) value)
    (make-mapping-node :pairs (nreverse pairs))))

(defmacro define-representer-dispatch (name clauses)
  "Define NAME from a declarative type/function table."
  `(defun ,name (value)
     (declare (optimize (speed 3) (safety 1)))
     (typecase value
       ,@(loop for (type function) in clauses
               collect `(,type (,function value)))
       (t (error 'yaml-emit-error)))))

(define-representer-dispatch represent
  ((null (lambda (value) (declare (ignore value))
           (make-sequence-node :items nil)))
   ((eql t) (lambda (value) (declare (ignore value))
              (%represent-scalar "true")))
   (yaml-sentinel (lambda (value)
                    (%represent-scalar (if (yaml-null-p value) "null" "false"))))
   (yaml-mapping %represent-mapping)
   (hash-table %represent-hash-table)
   (string %represent-scalar)
   (character (lambda (value) (%represent-scalar (string value))))
   (integer %represent-number)
   (float %represent-number)
   (vector %represent-vector)
   (cons %represent-list)))
