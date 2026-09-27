;;;; Explicit yaml-test-suite exclusions.
;;;;
;;;; Entries are (CASE-ID STAGE REASON), where STAGE is one of :READER,
;;;; :LOADER-ISOLATED, :LOADER-E2E, :DUMPER-E2E, or :EMITTER-ISOLATED.
;;;; Keep this file loadable when the external
;;;; fixture is absent.
(in-package #:cl-yaml-kit/test)

(defparameter *conformance-exclusions*
  '(("4fj6" :dumper-e2e "The input has a non-scalar mapping key; the loader cannot construct the value model for dumper round-trip comparison.")
    ("6bfj" :dumper-e2e "The input has a non-scalar mapping key; the loader cannot construct the value model for dumper round-trip comparison.")
    ("kk5p" :dumper-e2e "The input has a non-scalar mapping key; the loader cannot construct the value model for dumper round-trip comparison.")
    ("lx3p" :dumper-e2e "The input has a non-scalar mapping key; the loader cannot construct the value model for dumper round-trip comparison.")
    ("m5dy" :dumper-e2e "The input has a non-scalar mapping key; the loader cannot construct the value model for dumper round-trip comparison.")
    ("rzp5" :dumper-e2e "The input has a non-scalar mapping key; the loader cannot construct the value model for dumper round-trip comparison.")
    ("sbg9" :dumper-e2e "The input has a non-scalar mapping key; the loader cannot construct the value model for dumper round-trip comparison.")
    ("x38w" :dumper-e2e "The input has a non-scalar mapping key; the loader cannot construct the value model for dumper round-trip comparison.")
    ("xw4d" :dumper-e2e "The input has a non-scalar mapping key; the loader cannot construct the value model for dumper round-trip comparison.")))
