;;;; cl-yaml-kit.asd
(in-package #:asdf-user)

(asdf:defsystem "cl-yaml-kit"
  :description "YAML 1.2.2 reader and writer for Common Lisp"
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/cl-yaml-kit"
  :bug-tracker "https://github.com/nerima-lisp/cl-yaml-kit/issues"
  :source-control (:git "https://github.com/nerima-lisp/cl-yaml-kit.git")
  :depends-on ("cl-regex-kit" "cl-codec-kit") ; Regex resolves schema values; codec-kit decodes YAML byte streams.
  :pathname "src"
  :serial t
  :components ((:file "package") (:file "data") (:file "events")
               (:file "nodes") (:file "conditions")
               (:file "reader-macros") (:file "char-classes")
               (:file "tokens")
               (:file "scanner-state") (:file "scanner-scalars")
               (:file "scanner-structure") (:file "scanner")
               (:file "parser-states") (:file "parser")
               (:file "composer") (:file "schema") (:file "constructor")
               (:file "loader") (:file "representer") (:file "serializer")
               (:file "emitter-state") (:file "emitter-scalars")
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
  :depends-on ("cl-yaml-kit" "cl-weave" "cl-json-kit")
  :pathname "t"
  :serial t
  :components ((:file "package") (:file "helpers") (:file "events-test")
               (:file "nodes-test") (:file "reader-test")
               (:file "loader-test") (:file "dumper-test")
               (:file "conformance/events") (:file "conformance/values")
               (:file "conformance-test"))
  :perform (test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :cl-yaml-kit/test :run-tests)
               (error "cl-yaml-kit test suite failed"))))
