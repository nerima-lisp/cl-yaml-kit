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
   :max-depth max-depth
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
   :max-depth max-depth
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
(defun comment-spans (input &key
                              (max-input-length +default-max-input-length+))
  "Return source-ordered (START . END) spans, one for each comment recognized by
the YAML lexer. Offsets are zero-based character positions in the decoded input.
START is inclusive and END is exclusive. Spans include the leading # and comment
text, but exclude the line break."
  (let ((text (%parser-input-string input max-input-length))
        (spans nil))
    (when (> (length text) max-input-length)
      (error 'yaml-resource-limit-error :limit-name "input length"
             :limit max-input-length :actual (length text)))
    (let ((*scanner-comment-sink* (lambda (span) (push span spans)))
          (scanner (make-scanner text)))
      (loop while (scanner-next-token scanner)
            do (recycle-scanner-token scanner)))
    (nreverse spans)))

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
     :max-depth max-depth
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
