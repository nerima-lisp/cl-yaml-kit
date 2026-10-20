(in-package #:yaml-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %format-edit-container-case
      ((container) expression &body clauses)
    (let ((container-value (gensym "CONTAINER-"))
          (container-kind (gensym "KIND-")))
      `(let* ((,container-value ,expression)
              (,container-kind
                (cond
                  ((hash-table-p ,container-value) :hash-table)
                  ((yaml-mapping-p ,container-value) :yaml-mapping)
                  ((vectorp ,container-value) :vector)
                  ((listp ,container-value) :list)
                  (t :other))))
         (let ((,container ,container-value))
           (case ,container-kind ,@clauses)))))
  (declaim (inline %format-edit-sequence-index-p))

  (defun %format-edit-sequence-index-p (index length &optional require-end-p)
    (and (integerp index)
         (<= 0 index)
         (if require-end-p
             (= index length)
             (< index length)))))
