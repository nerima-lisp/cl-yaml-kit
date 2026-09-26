;;;; t/helpers.lisp
(in-package #:cl-yaml-kit/test)

(defun run-tests ()
  (run-all :reporter :spec :pass-with-no-tests nil))
