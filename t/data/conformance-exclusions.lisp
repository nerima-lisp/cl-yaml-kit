;;;; Explicit yaml-test-suite exclusions.
;;;;
;;;; Entries are (CASE-ID STAGE ...), where STAGE is one of :READER, :LOADER,
;;;; or :DUMPER.  Keep this file loadable when the external fixture is absent.
(defparameter *conformance-exclusions* nil)
