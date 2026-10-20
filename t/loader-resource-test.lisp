;;;; t/loader-resource-test.lisp
(in-package #:cl-yaml-kit/test)

(describe "loader resources"
  (cl-weave:it-each
      ((maximum-depth :compose-document
         ((:sequence-start) (:sequence-end))
         (:max-depth 0)
         (:name "depth" :limit 0 :actual 1))
       (maximum-nodes :compose-document
         ((:scalar :value "x"))
         (:max-nodes 0)
         (:name "nodes" :limit 0 :actual 1))
       (alias-expansions :compose-document
         ((:sequence-start :anchor "a")
          (:sequence-end)
          (:alias :anchor "a"))
         (:max-alias-expansions 0)
         (:name "alias expansions" :limit 0 :actual 1))
       (reader-input :parse-stream
         "x"
         (:max-input-length 0)
         (:name "input length" :limit 0 :actual 1)))
    "enforces loader resource limit ~S"
    (name operation input options expected)
    (declare (ignore name))
    (expect
     (lambda ()
       (if (eq operation :parse-stream)
           (with-input-from-string (stream input)
             (apply #'yaml-kit:parse (cons stream options)))
           (loader-case-call operation input options)))
     :to-signal-loader-resource-limit
     expected)))
