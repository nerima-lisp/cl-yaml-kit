;;;; Explicit yaml-test-suite exclusions.
;;;;
;;;; Entries are (CASE-ID STAGE REASON), where STAGE is one of :READER,
;;;; :LOADER-ISOLATED, :LOADER-E2E, :DUMPER-E2E, or :EMITTER-ISOLATED.
;;;; Keep this file loadable when the external
;;;; fixture is absent.
(in-package #:cl-yaml-kit/test)

(defparameter *conformance-exclusions* nil)
