;;;; Explicit yaml-test-suite exclusions.
;;;;
;;;; Entries are (CASE-ID STAGE REASON), where STAGE is one of :READER,
;;;; :LOADER, :DUMPER, or :EMITTER. Keep this file loadable when the external
;;;; fixture is absent.
(defparameter *conformance-exclusions* nil)
