;;;; run-tests.lisp
(require :asdf)
(defun script-directory ()
  (make-pathname :name nil :type nil
                 :defaults (or *load-truename* *compile-file-truename*
                               (error "Unable to determine script location"))))
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(script-directory)) :inherit-configuration))
(asdf:test-system "cl-yaml-kit")
(uiop:quit 0)
