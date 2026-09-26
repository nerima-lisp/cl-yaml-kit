;;;; t/package.lisp
(defpackage #:cl-yaml-kit/test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect #:run-all)
  (:export #:run-tests)
  (:import-from #:yaml-kit #:make-mark #:mark-line #:mark-column #:mark-offset
                #:make-scalar-event #:scalar-event-p #:scalar-event-value
                #:make-scalar-node #:scalar-node-p #:scalar-node-value
                #:yaml-parse-error #:yaml-parse-error-line))
(in-package #:cl-yaml-kit/test)
