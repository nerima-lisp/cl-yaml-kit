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
