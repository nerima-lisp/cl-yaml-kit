(defparameter *coverage-repro-nested*
  (progn
    (if *coverage-repro-false* (error "unexecuted initform"))
    (lambda ()
      (if *coverage-repro-false* (error "unexecuted lambda body")))))

(defun coverage-repro-nested-runtime-expressions-helper () (if nil :unreached :also-unreached))
