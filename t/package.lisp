;;;; t/package.lisp
(defpackage #:cl-yaml-kit/test
  (:use #:cl)
  (:shadowing-import-from #:cl-weave #:describe)
  (:import-from #:cl-weave #:it #:expect #:run-all)
  (:export #:run-tests)
  )
(in-package #:cl-yaml-kit/test)

(defvar *conformance-last-summaries* nil)
