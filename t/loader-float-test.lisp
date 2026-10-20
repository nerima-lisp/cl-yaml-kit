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

(progn
  (defun call-with-loader-floating-point-traps (thunk)
    (let ((modes (sb-int:get-floating-point-modes)))
      (unwind-protect
           (progn
             (sb-int:set-floating-point-modes
              :traps '(:invalid :overflow :divide-by-zero :underflow))
             (funcall thunk))
        (apply #'sb-int:set-floating-point-modes modes))))

  (defun loader-float-kind (value)
    (cond ((yaml-kit::%float-nan-p value) :nan)
          ((yaml-kit::%float-infinity-p value)
           (if (minusp value) :-inf :+inf))
          (t :finite)))

  (defun loader-float-kinds (values)
    (mapcar #'loader-float-kind values))

  (defun loader-float-mapping-values (value mapping-type)
    (case mapping-type
      (:hash-table (gethash "values" value))
      (:alist (cdr (assoc "values" value :test #'string=)))
      (:yaml-mapping
       (cdr (assoc "values"
                   (yaml-kit:yaml-mapping-entries value)
                   :test #'string=))))))

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
(cl-weave:it-each
    ((lowercase-nan ".nan" :nan)
     (mixed-case-nan ".NaN" :nan)
     (positive-infinity ".inf" :+inf)
     (negative-infinity "-.inf" :-inf))
  "handles special float scalar ~S with traps enabled"
  (name text expected-kind)
  (declare (ignore name))
  (call-with-loader-floating-point-traps
   (lambda ()
     (let ((value (yaml-kit:parse text)))
       (expect (loader-float-kind value) :to-equal expected-kind)
       (expect (loader-float-kind (yaml-kit:parse (yaml-kit:emit value)))
               :to-equal
               expected-kind)))))

(cl-weave:it-each
    ((positive-shortcut-overflow "1e401" :infinity nil)
     (negative-shortcut-overflow "-1e401" :infinity t)
     (positive-conversion-overflow "2e308" :infinity nil)
     (negative-conversion-overflow "-2e308" :infinity t)
     (positive-shortcut-underflow "!!float 1e-402" :zero nil)
     (negative-shortcut-underflow "!!float -1e-402" :zero t))
  "preserves float kind and sign at decimal range boundaries"
  (name text expected-kind negative)
  (declare (ignore name))
  (let ((value (yaml-kit:parse text)))
    (expect (and (floatp value)
                 (ecase expected-kind
                   (:infinity (yaml-kit::%float-infinity-p value))
                   (:zero (zerop value))))
            :to-be-truthy)
    (expect (minusp (float-sign value)) :to-equal negative)))

(cl-weave:it
 "preserves special float kinds in hash-table vectors with traps enabled"
 (call-with-loader-floating-point-traps
  (lambda ()
    (let* ((value
             (yaml-kit:parse "values: [.nan, .NaN, .inf, -.inf]"
                             :mapping-type :hash-table
                             :sequence-type :vector))
           (parsed-values (loader-float-mapping-values value :hash-table)))
      (expect (loader-float-kinds (coerce parsed-values 'list))
              :to-equal
              '(:nan :nan :+inf :-inf))))))

(cl-weave:it-each
    ((hash-table :hash-table)
     (alist :alist)
     (yaml-mapping :yaml-mapping))
  "preserves special float kinds in ~S mappings with traps enabled"
  (name mapping-type)
  (declare (ignore name))
  (call-with-loader-floating-point-traps
   (lambda ()
     (let* ((value
              (yaml-kit:parse "values: [.nan, .NaN, .inf, -.inf]"
                              :mapping-type mapping-type
                              :sequence-type :list))
            (round-trip
              (yaml-kit:parse (yaml-kit:emit value)
                             :mapping-type mapping-type
                             :sequence-type :list)))
       (expect (loader-float-kinds
                (loader-float-mapping-values value mapping-type))
               :to-equal
               '(:nan :nan :+inf :-inf))
       (expect (loader-float-kinds
                (loader-float-mapping-values round-trip mapping-type))
               :to-equal
               '(:nan :nan :+inf :-inf))))))
