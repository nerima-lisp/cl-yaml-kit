(in-package #:yaml-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro %define-format-edit-error (name condition parameters initargs)
    (let ((replacements
            (mapcar (lambda (parameter)
                      (cons parameter (gensym "ARGUMENT-")))
                    parameters)))
      `(defun ,name ,parameters
         (let* ,(mapcar (lambda (replacement)
                          `(,(cdr replacement) ,(car replacement)))
                        replacements)
           (error ',condition ,@(sublis replacements initargs))))))

  (defmacro %format-edit-container-case
      ((container kind) expression &body clauses)
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
         (let ((,container ,container-value)
               (,kind ,container-kind))
           (case ,kind ,@clauses)))))

  (defmacro %format-edit-sequence-index-p (index length &optional allow-end)
    (let ((index-value (gensym "INDEX-"))
          (length-value (gensym "LENGTH-"))
          (allow-end-value (gensym "ALLOW-END-")))
      `(let* ((,index-value ,index)
              (,length-value ,length)
              (,allow-end-value ,allow-end))
         (and (integerp ,index-value)
              (<= 0 ,index-value)
              (if ,allow-end-value
                  (= ,index-value ,length-value)
                  (< ,index-value ,length-value)))))))
