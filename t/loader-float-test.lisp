(in-package #:cl-yaml-kit/test)

(defun loader-float-oracle (text)
  (let ((*read-eval* nil)
        (*read-default-float-format* 'double-float))
    (car (multiple-value-list (read-from-string text)))))

(defun loader-float-text-generator ()
  (cl-weave:gen-tuple
   (cl-weave:gen-boolean)
   (cl-weave:gen-integer :min 0 :max 999999)
   (cl-weave:gen-integer :min 0 :max 999999)
   (cl-weave:gen-integer :min -20 :max 20)))

(cl-weave:it-property
 "parses decimal floats with one correctly rounded conversion"
 ((parts (loader-float-text-generator)))
 (destructuring-bind (negative integer fraction exponent) parts
   (let ((text (format nil "~A~D.~6,'0D~A~D"
                       (if negative "-" "") integer fraction
                       (if (minusp exponent) "e-" "e+")
                       (abs exponent))))
     (expect (yaml-kit::%parse-yaml-float text)
             :to-equal
             (loader-float-oracle text)))))

(cl-weave:it-each
    ((negative-zero "-0.0" "-0.0d0")
     (negative-integer-zero "-0" "-0.0d0"))
  "preserves the sign of zero-valued float scalars"
  (name text oracle-text)
  (declare (ignore name))
  (let ((actual (yaml-kit::%parse-yaml-float text))
        (oracle (loader-float-oracle oracle-text)))
    (expect (zerop actual) :to-be-truthy)
    (expect (minusp (float-sign actual))
            :to-equal
            (minusp (float-sign oracle)))))
(cl-weave:it
 "handles special floats with floating-point traps enabled"
 (let ((modes (sb-int:get-floating-point-modes)))
   (unwind-protect
        (progn
          (sb-int:set-floating-point-modes
           :traps '(:invalid :overflow :divide-by-zero :underflow))
          (labels ((kind (value)
                   (cond ((yaml-kit::%float-nan-p value) :nan)
                         ((yaml-kit::%float-infinity-p value)
                          (if (minusp value) :-inf :+inf))
                         (t :finite)))
                 (kinds (value)
                   (mapcar #'kind value))
                 (values-for (value mapping-type)
                   (case mapping-type
                     (:hash-table (gethash "values" value))
                     (:alist (cdr (assoc "values" value :test #'string=)))
                     (:yaml-mapping
                      (cdr (assoc "values"
                                  (yaml-kit:yaml-mapping-entries value)
                                  :test #'string=))))))
            (dolist (case '((".nan" :nan)
                            (".NaN" :nan)
                            (".inf" :+inf)
                            ("-.inf" :-inf)))
              (let* ((text (first case))
                     (expected (second case))
                     (value (yaml-kit:parse text)))
                (expect (kind value) :to-equal expected)
                (expect (kind (yaml-kit:parse (yaml-kit:emit value)))
                        :to-equal expected)))
            (let ((value (yaml-kit:parse "values: [.nan, .NaN, .inf, -.inf]"
                                         :mapping-type :hash-table
                                         :sequence-type :vector)))
              (expect (kinds (coerce (values-for value :hash-table) 'list))
                      :to-equal '(:nan :nan :+inf :-inf)))
            (dolist (mapping-type '(:hash-table :alist :yaml-mapping))
              (let ((value (yaml-kit:parse "values: [.nan, .NaN, .inf, -.inf]"
                                           :mapping-type mapping-type
                                           :sequence-type :list)))
                (expect (kinds (values-for value mapping-type))
                        :to-equal '(:nan :nan :+inf :-inf))
                (expect (kinds (values-for
                                (yaml-kit:parse (yaml-kit:emit value)
                                                :mapping-type mapping-type
                                                :sequence-type :list)
                                mapping-type))
                        :to-equal '(:nan :nan :+inf :-inf))))))
     (apply #'sb-int:set-floating-point-modes modes))))
