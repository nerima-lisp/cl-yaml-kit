;;;; src/representer.lisp
(in-package #:yaml-kit)

(defun %represent-scalar (value)
  (make-scalar-node :value value))

(defun %represent-number (value)
  (make-scalar-node :value
                    (cond ((and (floatp value) (sb-ext:float-infinity-p value))
                           (if (plusp (float-sign value)) ".inf" "-.inf"))
                          ((and (floatp value) (sb-ext:float-nan-p value)) ".nan")
                          (t (princ-to-string value)))))

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
  "Define NAME from a declarative ordered predicate/function table."
  `(defun ,name (value)
     (declare (optimize (speed 3) (safety 1)))
     (cond
       ,@(loop for (predicate function) in clauses
               collect `((,predicate value) (,function value))))))

(define-representer-dispatch represent
  ((yaml-null-p (lambda (value) (%represent-scalar "null")))
   (yaml-false-p (lambda (value) (%represent-scalar "false")))
   ((lambda (value) (null value)) (lambda (value) (%represent-scalar "null")))
   ((lambda (value) (eq value t)) (lambda (value) (%represent-scalar "true")))
   ((lambda (value) (typep value 'yaml-mapping)) %represent-mapping)
   ((lambda (value) (hash-table-p value)) %represent-hash-table)
   ((lambda (value) (stringp value)) %represent-scalar)
   ((lambda (value) (integerp value)) %represent-number)
   ((lambda (value) (floatp value)) %represent-number)
   ((lambda (value) (vectorp value)) %represent-vector)
   ((lambda (value) (consp value)) %represent-list)
   ((lambda (value) t) (lambda (value) (%represent-scalar (princ-to-string value))))))
