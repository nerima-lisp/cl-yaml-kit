;;;; src/package.lisp
(defpackage #:yaml-kit
  (:use #:cl)
  (:export
   ;; Reading and writing
   #:parse #:parse-all #:read-yaml #:parse-events #:map-events
   #:compose #:compose-all #:compose-events #:compose-all-events
   #:emit #:write-yaml #:emit-events
   ;; Sentinel and ordered mapping data
   #:+yaml-null+ #:+yaml-false+ #:yaml-null-p #:yaml-false-p
   #:make-yaml-mapping #:yaml-mapping-p #:yaml-mapping-entries
   ;; Marks, nodes, and node accessors
   #:mark #:make-mark #:mark-line #:mark-column #:mark-offset
   #:token #:make-token #:token-kind #:token-start-mark #:token-end-mark
   #:token-value #:token-handle #:token-suffix #:token-style
   #:token-major #:token-minor
   #:make-scanner #:scanner-peek-token #:scanner-next-token
   #:scalar-node #:make-scalar-node #:scalar-node-p
   #:sequence-node #:make-sequence-node #:sequence-node-p
   #:mapping-node #:make-mapping-node #:mapping-node-p
   #:node-tag #:node-anchor #:node-style #:node-start-mark #:node-end-mark
   #:scalar-node-value #:sequence-node-items #:mapping-node-pairs
   ;; Events and event accessors
   #:define-event #:stream-start-event #:stream-end-event
   #:make-stream-start-event #:make-stream-end-event
   #:document-start-event #:document-end-event #:sequence-start-event
   #:sequence-end-event #:mapping-start-event #:mapping-end-event
   #:scalar-event #:alias-event #:make-document-start-event
   #:make-document-end-event #:make-sequence-start-event
   #:make-sequence-end-event #:make-mapping-start-event
   #:make-mapping-end-event #:make-scalar-event #:make-alias-event
   #:stream-start-event-p #:stream-end-event-p #:document-start-event-p
   #:document-end-event-p #:sequence-start-event-p #:sequence-end-event-p
   #:mapping-start-event-p #:mapping-end-event-p #:scalar-event-p #:alias-event-p
   #:event-start-mark #:event-end-mark
   #:document-start-event-explicit-p #:document-start-event-version
   #:document-start-event-tag-directives #:document-end-event-explicit-p
   #:sequence-start-event-anchor #:sequence-start-event-tag
   #:sequence-start-event-implicit-p #:sequence-start-event-style
   #:mapping-start-event-anchor #:mapping-start-event-tag
   #:mapping-start-event-implicit-p #:mapping-start-event-style
   #:scalar-event-anchor #:scalar-event-tag #:scalar-event-value
   #:scalar-event-plain-implicit-p #:scalar-event-quoted-implicit-p
   #:scalar-event-style #:alias-event-anchor
   ;; Conditions
   #:yaml-kit-error #:yaml-parse-error #:yaml-parse-error-line
   #:yaml-parse-error-column #:yaml-parse-error-offset #:yaml-parse-error-context
   #:yaml-compose-error #:yaml-emit-error #:yaml-resource-limit-error))
