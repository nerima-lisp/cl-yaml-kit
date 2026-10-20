;;;; src/package.lisp
(defpackage #:yaml-kit
  (:use #:cl)
  (:export
   ;; Reading and event composition
   #:comment-spans #:comment-spans/k
   #:parse #:parse/k #:parse-all #:parse-all/k
   #:read-yaml #:read-yaml/k
   #:parse-events #:parse-events/k #:map-events #:map-events/k
   #:compose #:compose/k #:compose-all #:compose-all/k
   #:compose-events #:compose-events/k
   #:compose-all-events #:compose-all-events/k
   ;; Writing
   #:emit #:emit/k #:write-yaml #:write-yaml/k
   #:emit-events #:emit-events/k
   ;; Sentinel and ordered mapping data
   #:+yaml-null+ #:+yaml-false+ #:yaml-null-p #:yaml-false-p
   #:make-yaml-mapping #:yaml-mapping-p #:yaml-mapping-entries
   ;; Tokens, nodes, and node accessors
   #:token #:make-token #:token-kind #:token-start-span #:token-end-span
   #:token-value #:token-handle #:token-suffix #:token-style
   #:token-major #:token-minor
   #:scalar-node #:make-scalar-node #:scalar-node-p
   #:sequence-node #:make-sequence-node #:sequence-node-p
   #:mapping-node #:make-mapping-node #:mapping-node-p
   #:node-tag #:node-anchor #:node-style #:node-start-span #:node-end-span
   #:scalar-node-value #:sequence-node-items #:mapping-node-pairs
   ;; Events and event accessors
   #:stream-start-event #:stream-end-event
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
   #:event-start-span #:event-end-span
   #:document-start-event-explicit-p #:document-start-event-version
   #:document-start-event-tag-directives #:document-end-event-explicit-p
   #:sequence-start-event-anchor #:sequence-start-event-tag
   #:sequence-start-event-implicit-p #:sequence-start-event-style
   #:mapping-start-event-anchor #:mapping-start-event-tag
   #:mapping-start-event-implicit-p #:mapping-start-event-style
   #:scalar-event-anchor #:scalar-event-tag #:scalar-event-value
   #:scalar-event-plain-implicit-p #:scalar-event-quoted-implicit-p
   #:scalar-event-style #:alias-event-anchor
   ;; Format-preserving editing
   #:edit-source #:edit-source/k
   ;; Conditions
   #:yaml-kit-error #:yaml-kit-error-diagnostic
   #:yaml-parse-error #:yaml-parse-error-context
   #:yaml-compose-error #:yaml-compose-error-context
   #:yaml-compose-error-cause
   #:yaml-emit-error #:yaml-emit-error-context
   #:yaml-emit-error-cause
   #:yaml-resource-limit-error
   #:yaml-format-edit-path-error #:yaml-format-edit-path-error-path
   #:yaml-format-edit-path-error-document #:yaml-format-edit-path-error-operation
   #:yaml-format-edit-structure-error #:yaml-format-edit-structure-error-path
   #:yaml-format-edit-anchor-error #:yaml-format-edit-anchor-error-path
   #:yaml-format-edit-anchor-error-anchor))
