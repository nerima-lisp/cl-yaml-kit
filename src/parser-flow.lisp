(in-package #:yaml-kit)

(define-parser-state yaml-parser-parse-flow-sequence-first-entry (parser)
  (let ((token (parser-peek parser)))
    (cond
      ((eq (token-kind token) :flow-sequence-end)
       (parser-close-collection parser token #'make-sequence-end-event
                                (token-end-span token)))
      ((eq (token-kind token) :key)
       (parser-next parser)
       (parser-emit parser
                    (make-mapping-start-event :start-span (token-start-span token)
                                              :end-span (token-end-span token)
                                              :implicit-p t :style :flow))
       (parser-push-node-state parser
                               (#'yaml-parser-parse-flow-sequence-entry
                                #'yaml-parser-parse-flow-sequence-entry-mapping-end)
                               #'yaml-parser-parse-flow-sequence-entry-mapping-key-state))
      ;; c-s-implicit-json-key: a ":" that opens an entry gives the pair an
      ;; empty JSON-like key.  Only the first entry can do this; a ":" after a
      ;; node without a "," is a key spanning lines, which the spec forbids.
      ((eq (token-kind token) :value)
       (let ((span (token-start-span token)))
         (parser-emit parser
                      (make-mapping-start-event :start-span span
                                                :end-span (token-end-span token)
                                                :implicit-p t :style :flow))
         (parser-push-node-state parser
                                 (#'yaml-parser-parse-flow-sequence-entry
                                  #'yaml-parser-parse-flow-sequence-entry-mapping-end)
                                 (parser-empty-scalar-state
                                  parser span
                                  #'yaml-parser-parse-flow-sequence-entry-mapping-value))))
      (t
       (parser-push-node-state parser
                               (#'yaml-parser-parse-flow-sequence-entry)
                               #'yaml-parser-parse-node-flow)))))

(define-parser-state yaml-parser-parse-flow-sequence-entry (parser)
  (let ((token (parser-peek parser)))
    (cond
      ((eq (token-kind token) :flow-entry)
       (parser-next parser)
       #'yaml-parser-parse-flow-sequence-first-entry)
      ((eq (token-kind token) :flow-sequence-end)
       (parser-close-collection parser token #'make-sequence-end-event
                                (token-end-span token)))
      (t
       (parser-error token "did not find expected ',' or ']'")))))

(define-parser-state yaml-parser-parse-flow-sequence-entry-mapping-value (parser)
  (let ((token (parser-peek parser)))
    (if (eq (token-kind token) :value)
        (progn
          (parser-next parser)
          (if (member (token-kind (parser-peek parser))
                      '(:flow-entry :flow-sequence-end))
              #'yaml-parser-parse-flow-sequence-entry-mapping-end
              #'yaml-parser-parse-node-flow))
        (parser-empty-scalar-state parser (token-start-span token)
                                   #'yaml-parser-parse-flow-sequence-entry-mapping-end))))

(define-parser-state yaml-parser-parse-flow-sequence-entry-mapping-key-state (parser)
  (let ((token (parser-peek parser)))
    (if (member (token-kind token) '(:value :flow-entry :flow-sequence-end))
        (parser-empty-scalar-state parser (token-start-span token)
                                   #'yaml-parser-parse-flow-sequence-entry-mapping-value)
        (parser-push-node-state parser
                                (#'yaml-parser-parse-flow-sequence-entry-mapping-value)
                                #'yaml-parser-parse-node-flow))))

(define-parser-state yaml-parser-parse-flow-sequence-entry-mapping-end (parser)
  (let ((token (parser-peek parser)))
    (parser-emit parser (make-mapping-end-event :start-span (token-start-span token)
                                                :end-span (token-end-span token))))
  (parser-pop parser))

(define-parser-state yaml-parser-parse-flow-mapping-first-key (parser)
  (let ((token (parser-peek parser)))
    (cond
      ((eq (token-kind token) :flow-mapping-end)
       (parser-close-collection parser token #'make-mapping-end-event
                                (token-end-span token)))
      ;; c-ns-flow-map-empty-key-entry: a ":" where a key belongs gives the
      ;; entry an empty key and stays in this mapping.  Only reachable right
      ;; after "{" or ","; a ":" after a node would be a key spanning lines,
      ;; which the spec forbids.
      ((eq (token-kind token) :value)
       (parser-empty-scalar-state parser (token-start-span token)
                                  #'yaml-parser-parse-flow-mapping-value))
      ((eq (token-kind token) :key)
       (let ((next-token (progn (parser-next parser) (parser-peek parser))))
         (if (member (token-kind next-token) '(:value :flow-entry :flow-mapping-end))
             (parser-empty-scalar-state parser (token-start-span next-token)
                                        #'yaml-parser-parse-flow-mapping-value)
             (parser-push-node-state parser
                                     (#'yaml-parser-parse-flow-mapping-value)
                                     #'yaml-parser-parse-node-flow))))
      (t
       (parser-push-node-state parser
                               (#'yaml-parser-parse-flow-mapping-value)
                               #'yaml-parser-parse-node-flow)))))

(define-parser-state yaml-parser-parse-flow-mapping-key (parser)
  (let ((token (parser-peek parser)))
    (cond
      ((eq (token-kind token) :flow-entry)
       (parser-next parser)
       #'yaml-parser-parse-flow-mapping-first-key)
      ((eq (token-kind token) :flow-mapping-end)
       (parser-close-collection parser token #'make-mapping-end-event
                                (token-end-span token)))
      (t
       (parser-error token "did not find expected ',' or '}'")))))

(define-parser-state yaml-parser-parse-flow-mapping-value (parser)
  (let ((token (parser-peek parser)))
    (when (eq (token-kind token) :value)
      (parser-next parser))
    (if (member (token-kind (parser-peek parser))
                '(:flow-entry :flow-mapping-end))
        (parser-empty-scalar-state parser (token-start-span (parser-peek parser))
                                   #'yaml-parser-parse-flow-mapping-key)
        (parser-push-node-state parser
                                (#'yaml-parser-parse-flow-mapping-key)
                                #'yaml-parser-parse-node-flow))))
