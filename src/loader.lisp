;;;; src/loader.lisp
(in-package #:yaml-kit)

(defun %loader-compose-options (input max-input-length max-depth max-scalar-length
                                max-nodes max-alias-expansions)
  (append (unless (listp input)
            (list :max-input-length max-input-length
                  :max-scalar-length max-scalar-length))
          (list :max-depth max-depth
                :max-nodes max-nodes
                :max-alias-expansions max-alias-expansions)))

(defun compose (input &key (max-input-length 104857600) (max-depth 1000)
                             (max-scalar-length 16777216)
                             (max-nodes 1000000)
                             (max-alias-expansions 100000))
  "Return the first representation graph in INPUT.

INPUT may be an event list for compatibility, or a reader input accepted by
MAP-EVENTS.  Non-list input is passed to the composer without materializing
an intermediate event list."
  (apply #'compose-events
         (cons input
               (%loader-compose-options input max-input-length max-depth max-scalar-length
                                        max-nodes max-alias-expansions))))

(defun compose-all (input &key (max-input-length 104857600) (max-depth 1000)
                                 (max-scalar-length 16777216)
                                 (max-nodes 1000000)
                                 (max-alias-expansions 100000))
  "Return all representation graphs in INPUT.

INPUT may be an event list for compatibility, or a reader input accepted by
MAP-EVENTS.  Non-list input is passed to the composer without materializing
an intermediate event list."
  (apply #'compose-all-events
         (cons input
               (%loader-compose-options input max-input-length max-depth max-scalar-length
                                        max-nodes max-alias-expansions))))

(defun parse (input &key (schema :core) (mapping-type :hash-table)
                            (sequence-type :vector)
                            (duplicate-key-policy :error)
                            (max-input-length 104857600) (max-depth 1000)
                            (max-scalar-length 16777216)
                            (max-nodes 1000000)
                            (max-alias-expansions 100000))
  "Parse the first YAML document from INPUT.

Non-list input is consumed by MAP-EVENTS through the composer, so no
intermediate event list is created."
  (construct
   (apply #'compose
          (cons input
                (%loader-compose-options input max-input-length max-depth max-scalar-length
                                         max-nodes max-alias-expansions)))
   :schema schema :mapping-type mapping-type :sequence-type sequence-type
   :duplicate-key-policy duplicate-key-policy))

(defun parse-all (input &key (schema :core) (mapping-type :hash-table)
                                (sequence-type :vector)
                                (duplicate-key-policy :error)
                                (max-input-length 104857600) (max-depth 1000)
                                (max-scalar-length 16777216)
                                (max-nodes 1000000)
                                (max-alias-expansions 100000))
  "Parse every YAML document from INPUT.

Non-list input is consumed by MAP-EVENTS through the composer, so no
intermediate event list is created."
  (mapcar (lambda (node)
            (construct node :schema schema :mapping-type mapping-type
                            :sequence-type sequence-type
                            :duplicate-key-policy duplicate-key-policy))
          (apply #'compose-all
                 (cons input
                       (%loader-compose-options input max-input-length max-depth
                                                max-scalar-length max-nodes
                                                max-alias-expansions)))))

(defun read-yaml (stream &key (schema :core) (mapping-type :hash-table)
                              (sequence-type :vector)
                              (duplicate-key-policy :error)
                              (max-input-length 104857600) (max-depth 1000)
                              (max-scalar-length 16777216)
                              (max-nodes 1000000)
                              (max-alias-expansions 100000))
  "Read and parse one YAML document from STREAM."
  (parse stream :schema schema :mapping-type mapping-type
               :sequence-type sequence-type
               :duplicate-key-policy duplicate-key-policy
               :max-input-length max-input-length :max-depth max-depth
               :max-scalar-length max-scalar-length :max-nodes max-nodes
               :max-alias-expansions max-alias-expansions))
