# API Reference

The public package is `yaml-kit`. All symbols below are exported by
`src/package.lisp`. Optional arguments shown with a default use the default
implemented by the source.

## Reading and composing

| Symbol and lambda list | Returns | Signals |
| --- | --- | --- |
| `parse input &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | First constructed document | `yaml-parse-error`, `yaml-compose-error`, `yaml-resource-limit-error` |
| `parse-all input &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | List of constructed documents | Same as `parse` |
| `read-yaml stream &key schema mapping-type sequence-type duplicate-key-policy max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | One constructed document | Same as `parse` |
| `parse-events input &rest keys` | Event list | `yaml-parse-error`, `yaml-resource-limit-error` |
| `map-events handler input &key max-input-length max-depth max-scalar-length` | `nil` | `yaml-parse-error`, `yaml-resource-limit-error` |
| `compose-events events &rest keys` | First representation graph | `yaml-compose-error`, `yaml-resource-limit-error` |
| `compose-all-events events &rest keys` | Representation graphs | `yaml-compose-error`, `yaml-resource-limit-error` |
| `compose input &key max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | First representation node | `yaml-compose-error`, `yaml-resource-limit-error` |
| `compose-all input &key max-input-length max-depth max-scalar-length max-nodes max-alias-expansions` | List of representation nodes | Same as `compose` |

`input` may be a string, octet vector accepted by the reader, stream, or event
list where the implementation permits it. Defaults are `:core`,
`:hash-table`, `:vector`, `:error`, 104857600 input characters, depth 1000,
scalar length 16777216, 1000000 nodes, and 100000 alias expansions. Event
mapping defaults are input length 104857600, depth 256, and scalar length
16777216.

## Emitting

| Symbol and lambda list | Returns | Signals |
| --- | --- | --- |
| `emit value &key indent default-flow-style explicit-document-start` | YAML string | `yaml-emit-error` |
| `write-yaml value stream &key indent default-flow-style explicit-document-start` | `value` | `yaml-emit-error` |
| `emit-events events &key indent explicit-document-start` | YAML string | `yaml-emit-error` |

Defaults are indent 2, `:block`, and false for explicit document start.

## Sentinel and mapping values

| Symbol | Lambda list or value | Returns |
| --- | --- | --- |
| `+yaml-null+`, `+yaml-false+` | special variables | Opaque null and false markers |
| `yaml-null-p`, `yaml-false-p` | `(value)` | Boolean identity test |
| `make-yaml-mapping` | `(&optional entries)` | New ordered mapping |
| `yaml-mapping-p` | `(object)` | Boolean |
| `yaml-mapping-entries` | `(mapping)` | Mapping entry alist |

## Marks, tokens, and scanners

| Symbol | Lambda list or access | Result |
| --- | --- | --- |
| `mark` | structure type | Mark structure |
| `make-mark` | `(line column offset)` | Mark |
| `mark-line`, `mark-column`, `mark-offset` | `(mark)` | Nonnegative integer |
| `token` | structure type | Token structure |
| `make-token` | `(kind start-mark end-mark &key value handle suffix style major minor)` | Validated token |
| `token-kind`, `token-start-mark`, `token-end-mark`, `token-value`, `token-handle`, `token-suffix`, `token-style`, `token-major`, `token-minor` | `(token)` | Corresponding token field |
| `make-scanner` | `(simple-character-array)` | Scanner |
| `scanner-peek-token`, `scanner-next-token` | `(scanner)` | Next token or `nil` |

## Nodes

`make-scalar-node` accepts the base node keywords `:tag :anchor :style
:start-mark :end-mark` plus `:value`. `make-sequence-node` additionally accepts
`:items`; `make-mapping-node` additionally accepts `:pairs`. The predicates
`scalar-node-p`, `sequence-node-p`, and `mapping-node-p` accept one object and
return a boolean. `node-tag`, `node-anchor`, `node-style`, `node-start-mark`,
`node-end-mark`, `scalar-node-value`, `sequence-node-items`, and
`mapping-node-pairs` accept the corresponding node and return its field.

`scalar-node`, `sequence-node`, and `mapping-node` are the structure types.
`define-event` is the macro used to define event structures; it accepts
`(name slots &optional documentation)` and returns the name.

## Events

The constructors all accept the base keywords `:start-mark :end-mark`.
`make-document-start-event` adds `:explicit-p :version :tag-directives`;
`make-document-end-event` adds `:explicit-p`; sequence and mapping start
constructors add `:anchor :tag :implicit-p :style`; the scalar constructor
adds `:anchor :tag :value :plain-implicit-p :quoted-implicit-p :style`; and
`make-alias-event` adds `:anchor`.

The event types are `stream-start-event`, `stream-end-event`,
`document-start-event`, `document-end-event`, `sequence-start-event`,
`sequence-end-event`, `mapping-start-event`, `mapping-end-event`,
`scalar-event`, and `alias-event`. Their `*-p` predicates accept one object.
`event-start-mark` and `event-end-mark` accept any event. The remaining event
readers accept their named event and return the corresponding constructor
field: `document-start-event-explicit-p`, `document-start-event-version`,
`document-start-event-tag-directives`, `document-end-event-explicit-p`,
`sequence-start-event-anchor`, `sequence-start-event-tag`,
`sequence-start-event-implicit-p`, `sequence-start-event-style`,
`mapping-start-event-anchor`, `mapping-start-event-tag`,
`mapping-start-event-implicit-p`, `mapping-start-event-style`,
`scalar-event-anchor`, `scalar-event-tag`, `scalar-event-value`,
`scalar-event-plain-implicit-p`, `scalar-event-quoted-implicit-p`,
`scalar-event-style`, and `alias-event-anchor`.

## Conditions

`yaml-kit-error` is the base condition. `yaml-parse-error`,
`yaml-compose-error`, `yaml-emit-error`, and `yaml-resource-limit-error` are
its exported subtypes. Their readers and defaults are specified in
[Conditions](conditions.md).
