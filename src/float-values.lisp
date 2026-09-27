(in-package #:yaml-kit)

(defun %float-positive-infinity ()
  #+sbcl sb-kernel::double-float-positive-infinity
  #-sbcl (error "cl-yaml-kit requires SBCL"))

(defun %float-negative-infinity ()
  #+sbcl sb-kernel::double-float-negative-infinity
  #-sbcl (error "cl-yaml-kit requires SBCL"))

(defun %float-nan ()
  #+sbcl (sb-kernel:make-double-float #x7ff80000 1)
  #-sbcl (error "cl-yaml-kit requires SBCL"))

(defun %float-infinity-p (value)
  #+sbcl (sb-ext:float-infinity-p value)
  #-sbcl nil)

(defun %float-nan-p (value)
  #+sbcl (sb-ext:float-nan-p value)
  #-sbcl nil)

(defun %scan-yaml-float-exponent (text index length mark)
  (incf index)
  (let ((sign 1) (exponent 0))
    (when (and (< index length)
               (member (char text index) '(#\+ #\-) :test #'char=))
      (when (char= (char text index) #\-)
        (setf sign -1))
      (incf index))
    (unless (and (< index length) (digit-char-p (char text index) 10))
      (%float-error mark))
    (loop while (and (< index length)
                     (digit-char-p (char text index) 10))
          do (setf exponent
                   (min 1001 (+ (* exponent 10)
                                (digit-char-p (char text index) 10))))
             (incf index))
    (values index sign exponent)))

(defun %scan-yaml-float-digits (text index length mantissa
                                significant-digits saw-significant-digit)
  (let ((count 0))
    (loop while (and (< index length)
                     (digit-char-p (char text index) 10))
          do (let ((digit (digit-char-p (char text index) 10)))
               (setf mantissa (+ (* mantissa 10) digit))
               (when (or saw-significant-digit (plusp digit))
                 (setf saw-significant-digit t)
                 (incf significant-digits))
               (incf count)
               (incf index)))
    (values index mantissa significant-digits saw-significant-digit count)))

(defun %scan-yaml-float (text mark)
  (let* ((lower (string-downcase text))
         (length (length lower))
         (index 0)
         (sign 1)
         (mantissa 0)
         (fraction-digits 0)
         (exponent 0)
         (exponent-sign 1)
         (saw-significant-digit nil)
         (significant-digits 0)
         (total-digits 0))
    (when (and (< index length)
               (member (char lower index) '(#\+ #\-) :test #'char=))
      (when (char= (char lower index) #\-)
        (setf sign -1))
      (incf index))
    (multiple-value-bind (new-index new-mantissa new-significant
                          new-saw-significant integer-digits)
        (%scan-yaml-float-digits lower index length mantissa
                                 significant-digits saw-significant-digit)
      (setf index new-index mantissa new-mantissa
            significant-digits new-significant
            saw-significant-digit new-saw-significant
            total-digits integer-digits))
    (when (and (< index length) (char= (char lower index) #\.))
      (incf index)
      (multiple-value-bind (new-index new-mantissa new-significant
                            new-saw-significant digits)
          (%scan-yaml-float-digits lower index length mantissa
                                   significant-digits saw-significant-digit)
        (setf index new-index mantissa new-mantissa
              significant-digits new-significant
              saw-significant-digit new-saw-significant
              fraction-digits digits
              total-digits (+ total-digits digits))))
    (let ((saw-digit (plusp total-digits)))
      (when (and (< index length) (char= (char lower index) #\e))
        (multiple-value-setq (index exponent-sign exponent)
          (%scan-yaml-float-exponent lower index length mark)))
      (unless (and saw-digit (= index length))
        (%float-error mark))
      (values sign mantissa
              (- (* exponent-sign exponent) fraction-digits)
              significant-digits))))

(defun %decimal-rational-to-double (sign mantissa scale significant-digits)
  (cond
    ((zerop mantissa)
     (if (minusp sign) (* -1d0 0d0) 0d0))
    ((> (+ significant-digits scale) 400)
     (if (minusp sign) (%float-negative-infinity)
         (%float-positive-infinity)))
    ((< (+ significant-digits scale) -400)
     (if (minusp sign) (* -1d0 0d0) 0d0))
    (t
     (handler-case
         (let ((rational (* sign mantissa
                            (if (minusp scale)
                                (/ 1 (expt 10 (- scale)))
                                (expt 10 scale)))))
           (float rational 1d0))
       (floating-point-overflow ()
         (if (minusp sign) (%float-negative-infinity)
             (%float-positive-infinity)))))))

(defun %parse-yaml-float (text &optional mark)
  (let ((lower (string-downcase text)))
    (cond
      ((string= lower ".nan") (%float-nan))
      ((member lower '(".inf" "+.inf") :test #'string=)
       (%float-positive-infinity))
      ((string= lower "-.inf") (%float-negative-infinity))
      (t
       (multiple-value-call #'%decimal-rational-to-double
         (%scan-yaml-float text mark))))))

(defun %float-error (mark)
  (signal-yaml-compose-error :mark mark :cause "invalid scalar value"))
