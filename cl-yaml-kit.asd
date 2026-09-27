;;;; cl-yaml-kit.asd
(in-package #:asdf-user)

(unless (member :sbcl *features*)
  (error "cl-yaml-kit v0.1.0 supports SBCL only"))

(asdf:defsystem "cl-yaml-kit"
  :description "YAML 1.2.2 reader and writer for Common Lisp"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-yaml-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-yaml-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-yaml-kit.git")
  :depends-on ("cl-regex-kit"  ; Schema regexes resolve scalar values (schema.lisp)
                "cl-codec-kit") ; Detects and decodes YAML octet input (parser-entry.lisp)
  :pathname "src"
  :serial t
  :components ((:file "package") (:file "data") (:file "events")
               (:file "nodes") (:file "conditions")
               (:file "char-classes")
               (:file "tokens")
               (:file "scanner-state") (:file "scanner-directives")
               (:file "scanner-block-scalars") (:file "scanner-flow-scalars")
               (:file "scanner-fetch")
               (:file "scanner")
               (:file "parser-states") (:file "parser")
               (:file "parser-flow") (:file "parser-entry")
               (:file "composer") (:file "schema") (:file "constructor")
               (:file "float-values")
               (:file "constructor-collections")
               (:file "loader") (:file "representer") (:file "serializer")
               (:file "emitter-state") (:file "emitter-scalars")
               (:file "emitter-writers")
               (:file "emitter-directives")
               (:file "emitter-frames") (:file "emitter-events")
               (:file "emitter") (:file "dumper"))
  :in-order-to ((test-op (test-op "cl-yaml-kit/test"))))

(asdf:defsystem "cl-yaml-kit/test"
  :description "Tests for cl-yaml-kit"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-yaml-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-yaml-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-yaml-kit.git")
  :depends-on ("cl-yaml-kit"  ; The system under test.
                "cl-weave"    ; Test framework, generators, per-test timeout.
                "cl-json-kit") ; Reads the suite's in.json, telling null from false.
  :pathname "t"
  :serial t
  :components ((:file "package") (:file "helpers") (:file "events-test")
               (:file "nodes-test") (:file "data-test")
               (:file "conditions-test") (:file "reader-test")
               (:file "scanner-test") (:file "scanner-directives-test")
               (:file "parser-test")
               (:file "scanner-block-scalars-test")
               (:file "scanner-flow-scalars-test")
               (:file "scanner-fetch-test")
               (:file "loader-contract-helpers-test")
               (:file "loader-test") (:file "loader-schema-test")
               (:file "loader-float-test")
               (:file "loader-construction-test")
               (:file "loader-construction-errors-test")
               (:file "loader-resource-test")
               (:file "loader-integration-test") (:file "dumper-properties-test")
               (:file "dumper-test")
               (:file "conformance/events") (:file "conformance/values")
               (:file "conformance-test") (:file "conformance/stages")
               (:file "conformance/report") (:file "conformance/cases"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :cl-yaml-kit/test :run-tests)
               (error "cl-yaml-kit test suite failed"))))
