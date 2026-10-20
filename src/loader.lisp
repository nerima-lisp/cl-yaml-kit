;;;; src/loader.lisp
(in-package #:yaml-kit)

(defun %loader-event-source (input max-input-length max-depth max-scalar-length)
  (lambda (handler)
    (map-events handler input :max-input-length max-input-length
                :max-depth max-depth :max-scalar-length max-scalar-length)))

(define-yaml-resource-function compose (input)
    (:group :reader :keywords ())
  "Return the first representation graph in reader INPUT."
  (compose-events
   (%loader-event-source input max-input-length max-depth max-scalar-length)
   :max-depth max-depth
   :max-scalar-length max-scalar-length :max-nodes max-nodes
   :max-alias-expansions max-alias-expansions))

(define-yaml-resource-function compose-all (input)
    (:group :reader :keywords ())
  "Return all representation graphs in reader INPUT."
  (compose-all-events
   (%loader-event-source input max-input-length max-depth max-scalar-length)
   :max-depth max-depth
   :max-scalar-length max-scalar-length :max-nodes max-nodes
   :max-alias-expansions max-alias-expansions))

(define-yaml-resource-function parse (input)
    (:group :reader
     :keywords ((schema :core)
                (mapping-type :hash-table)
                (sequence-type :vector)
                (duplicate-key-policy :error)))
  "Parse the first YAML document from reader INPUT."
  (construct
   (compose input :max-input-length max-input-length :max-depth max-depth
                  :max-scalar-length max-scalar-length :max-nodes max-nodes
                  :max-alias-expansions max-alias-expansions)
   :schema schema :mapping-type mapping-type :sequence-type sequence-type
   :duplicate-key-policy duplicate-key-policy))
(define-yaml-resource-function comment-spans (input)
    (:group :input :keywords ())
  "Return source-ordered (START . END) spans, one for each comment recognized by
the YAML lexer. Offsets are zero-based character positions in the decoded input.
START is inclusive and END is exclusive. Spans include the leading # and comment
text, but exclude the line break."
  (let* ((decoded-text (%parser-input-string input max-input-length))
         (text (with-input-from-string (stream decoded-text)
                 (%parser-input-string stream max-input-length)))
         (spans nil))
    (let ((*scanner-comment-sink* (lambda (span) (push span spans)))
          (scanner (make-scanner text)))
      (loop while (scanner-next-token scanner)
            do (recycle-scanner-token scanner)))
    (nreverse spans)))

(define-yaml-resource-function parse-all (input)
    (:group :reader
     :keywords ((schema :core)
                (mapping-type :hash-table)
                (sequence-type :vector)
                (duplicate-key-policy :error)))
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

(define-yaml-resource-function read-yaml (stream)
    (:group :reader
     :keywords ((schema :core)
                (mapping-type :hash-table)
                (sequence-type :vector)
                (duplicate-key-policy :error)))
  "Read and parse one YAML document from STREAM."
  (parse stream :schema schema :mapping-type mapping-type
               :sequence-type sequence-type
               :duplicate-key-policy duplicate-key-policy
               :max-input-length max-input-length :max-depth max-depth
               :max-scalar-length max-scalar-length :max-nodes max-nodes
               :max-alias-expansions max-alias-expansions))
(define-yaml-resource-function comment-spans/k (input k)
    (:group :input :keywords ())
  "Call K with the source-ordered comment spans from INPUT."
  (check-type k function)
  (values
   (funcall k (comment-spans input :max-input-length max-input-length))))
(define-yaml-resource-function compose/k (input k)
    (:group :reader :keywords ())
  "Call K with the first representation graph from INPUT."
  (check-type k function)
  (values
   (funcall k
            (compose input
                     :max-input-length max-input-length
                     :max-depth max-depth
                     :max-scalar-length max-scalar-length
                     :max-nodes max-nodes
                     :max-alias-expansions max-alias-expansions))))
(define-yaml-resource-function compose-all/k (input k)
    (:group :reader :keywords ())
  "Call K with all representation graphs from INPUT."
  (check-type k function)
  (values
   (funcall k
            (compose-all input
                         :max-input-length max-input-length
                         :max-depth max-depth
                         :max-scalar-length max-scalar-length
                         :max-nodes max-nodes
                         :max-alias-expansions max-alias-expansions))))
(define-yaml-resource-function parse/k (input k)
    (:group :reader
     :keywords ((schema :core)
                (mapping-type :hash-table)
                (sequence-type :vector)
                (duplicate-key-policy :error)))
  "Call K with the first parsed document from INPUT."
  (check-type k function)
  (values
   (funcall k
            (parse input
                   :schema schema
                   :mapping-type mapping-type
                   :sequence-type sequence-type
                   :duplicate-key-policy duplicate-key-policy
                   :max-input-length max-input-length
                   :max-depth max-depth
                   :max-scalar-length max-scalar-length
                   :max-nodes max-nodes
                   :max-alias-expansions max-alias-expansions))))
(define-yaml-resource-function parse-all/k (input k)
    (:group :reader
     :keywords ((schema :core)
                (mapping-type :hash-table)
                (sequence-type :vector)
                (duplicate-key-policy :error)))
  "Call K with all parsed documents from INPUT."
  (check-type k function)
  (values
   (funcall k
            (parse-all input
                       :schema schema
                       :mapping-type mapping-type
                       :sequence-type sequence-type
                       :duplicate-key-policy duplicate-key-policy
                       :max-input-length max-input-length
                       :max-depth max-depth
                       :max-scalar-length max-scalar-length
                       :max-nodes max-nodes
                       :max-alias-expansions max-alias-expansions))))
(define-yaml-resource-function read-yaml/k (stream k)
    (:group :reader
     :keywords ((schema :core)
                (mapping-type :hash-table)
                (sequence-type :vector)
                (duplicate-key-policy :error)))
  "Call K with the parsed YAML document read from STREAM."
  (check-type k function)
  (values
   (funcall k
            (read-yaml stream
                       :schema schema
                       :mapping-type mapping-type
                       :sequence-type sequence-type
                       :duplicate-key-policy duplicate-key-policy
                       :max-input-length max-input-length
                       :max-depth max-depth
                       :max-scalar-length max-scalar-length
                       :max-nodes max-nodes
                       :max-alias-expansions max-alias-expansions))))
