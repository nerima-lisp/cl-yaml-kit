;;;; src/loader.lisp
(in-package #:yaml-kit)

(defun %loader-event-source (input max-input-length max-depth max-scalar-length)
  (lambda (handler)
    (map-events handler input :max-input-length max-input-length
                :max-depth max-depth :max-scalar-length max-scalar-length)))

(defun compose (input &key (max-input-length +default-max-input-length+)
                             (max-depth +default-max-depth+)
                             (max-scalar-length +default-max-scalar-length+)
                             (max-nodes +default-max-nodes+)
                             (max-alias-expansions +default-max-alias-expansions+))
  "Return the first representation graph in reader INPUT."
  (compose-events
   (%loader-event-source input max-input-length max-depth max-scalar-length)
   :max-input-length max-input-length :max-depth max-depth
   :max-scalar-length max-scalar-length :max-nodes max-nodes
   :max-alias-expansions max-alias-expansions))

(defun compose-all (input &key (max-input-length +default-max-input-length+)
                                 (max-depth +default-max-depth+)
                                 (max-scalar-length +default-max-scalar-length+)
                                 (max-nodes +default-max-nodes+)
                                 (max-alias-expansions +default-max-alias-expansions+))
  "Return all representation graphs in reader INPUT."
  (compose-all-events
   (%loader-event-source input max-input-length max-depth max-scalar-length)
   :max-input-length max-input-length :max-depth max-depth
   :max-scalar-length max-scalar-length :max-nodes max-nodes
   :max-alias-expansions max-alias-expansions))

(defun parse (input &key (schema :core) (mapping-type :hash-table)
                            (sequence-type :vector)
                            (duplicate-key-policy :error)
                            (max-input-length +default-max-input-length+)
                            (max-depth +default-max-depth+)
                            (max-scalar-length +default-max-scalar-length+)
                            (max-nodes +default-max-nodes+)
                            (max-alias-expansions +default-max-alias-expansions+))
  "Parse the first YAML document from reader INPUT."
  (construct
   (compose input :max-input-length max-input-length :max-depth max-depth
                  :max-scalar-length max-scalar-length :max-nodes max-nodes
                  :max-alias-expansions max-alias-expansions)
   :schema schema :mapping-type mapping-type :sequence-type sequence-type
   :duplicate-key-policy duplicate-key-policy))

(defun parse-all (input &key (schema :core) (mapping-type :hash-table)
                                (sequence-type :vector)
                                (duplicate-key-policy :error)
                                (max-input-length +default-max-input-length+)
                                (max-depth +default-max-depth+)
                                (max-scalar-length +default-max-scalar-length+)
                                (max-nodes +default-max-nodes+)
                                (max-alias-expansions +default-max-alias-expansions+))
  "Parse every YAML document from reader INPUT."
  (let ((values nil))
    (compose-all-events
     (%loader-event-source input max-input-length max-depth max-scalar-length)
     :max-input-length max-input-length :max-depth max-depth
     :max-scalar-length max-scalar-length :max-nodes max-nodes
     :max-alias-expansions max-alias-expansions
     :document-handler
     (lambda (node)
       (push (construct node :schema schema :mapping-type mapping-type
                              :sequence-type sequence-type
                              :duplicate-key-policy duplicate-key-policy)
             values)))
    (nreverse values)))

(defun read-yaml (stream &key (schema :core) (mapping-type :hash-table)
                              (sequence-type :vector)
                              (duplicate-key-policy :error)
                              (max-input-length +default-max-input-length+)
                              (max-depth +default-max-depth+)
                              (max-scalar-length +default-max-scalar-length+)
                              (max-nodes +default-max-nodes+)
                              (max-alias-expansions +default-max-alias-expansions+))
  "Read and parse one YAML document from STREAM."
  (parse stream :schema schema :mapping-type mapping-type
               :sequence-type sequence-type
               :duplicate-key-policy duplicate-key-policy
               :max-input-length max-input-length :max-depth max-depth
               :max-scalar-length max-scalar-length :max-nodes max-nodes
               :max-alias-expansions max-alias-expansions))
